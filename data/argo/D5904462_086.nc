CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-11-10T03:16:04Z AOML 3.0 creation; 2016-08-07T21:51:23Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151110031604  20160807145123  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               VA   AO  5287_9017_086                   2C  D   APEX                            6529                            072314                          846 @�}~���Z1   @�}O��@0��C���d��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    VA   B   B   @���@�  @���AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyl�D��D�I�D�|�D��3D�3D�@ D�� D�s3D��D�L�D�� Dǹ�D��fD�L�DچfD�ɚD�3D�@ D�` D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��A��A$��AF�\Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B�B���B���B���B�k�B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���C h�Ch�Ch�Ch�Ch�C
h�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C h�C"h�C$h�C&h�C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^��C`h�Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�AHC�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%=D%�=D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ �DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_ �D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt�=Dy�
D��D�V�D���D��RD�RD�MD��D��RD��D�Y�D��D�ƹD��D�Y�Dړ�D�ֹD� RD�MD�mD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�VA�S�A�K�A�G�A�E�A�G�A�G�A�G�A�G�A�E�A�E�A�?}A�1'A�7LA�9XA�9XA�5?A�7LA�7LA�9XA�9XA�9XA�;dA�9XA�9XA�;dA�;dA�=qA�=qA�?}A�=qA�9XA�7LA�5?A�/A�-A�/A�/A�/Aߟ�A�bNA�33A��A���A�;dA�~�A�
=A�7LA��Aɡ�A�$�A�Q�A���AƲ-AžwA�VA�JA��A���A��
A��A�33A��DA�"�A���A���A��A� �A���A���A�=qA� �A��A�5?A��A�jA���A�9XA�C�A��hA���A��A�n�A�M�A��A��uA��A�-A�ZA�ffA���A�r�A��/A�ZA��
A�ĜA�(�A�  A�ƨA���A��TA�mA{�-Aup�AqVAnE�Al��Aj��AgAd��AaA_oA[�AV��ATȴAT-ARȴAO��AMt�AK+AH�yAE�;ACoA@�/A>��A<ȴA;"�A8��A81A7�wA7dZA6�A3��A0�A-A+��A*9XA(�DA'"�A$�A$5?A#XA#/A!�FA ��A 5?A!+A"9XA$�RA%��A&��A&ĜA&(�A$��A#��A"��A"�\A!�A/AbNAZA��A�TA�-A�AoA7LA�\A�^A^5Av�A|�AZA�A��AXA�DA
�yA	��A	x�A�yA�\A�#A^5A��AhsA{A �HA �RA ��A �+A ��A33AƨAdZAG�AAdZA+A%A �RA M�@���@�^5@��@���@���@�M�@�dZ@�C�@�C�@�33@�|�@��P@�t�@�"�@��@�=q@��@���@�z�@�A�@��w@�C�@�ȴ@�{@��@�t�@���@�=q@�E�@��-@��9@�33@�n�@��#@�hs@�V@� �@�=q@�1'@���@��@�@��@陚@�?}@�hs@�9@�=q@�9@��@�^5@�`B@�O�@��@�Ĝ@�u@��;@��;@�@��y@�M�@�J@�I�@�=q@��@�\)@�^5@�hs@ؼj@�A�@ו�@��H@�ȴ@�ff@�7L@ԣ�@�ƨ@ӕ�@�dZ@�;d@�o@��y@��@�=q@��#@ѡ�@�x�@�@җ�@�ff@�
=@�o@���@��@��@Гu@�z�@�z�@�j@�Z@�1'@��m@�1@ϥ�@�@��H@�ȴ@�~�@��@�O�@�V@��@�Ĝ@�Z@��
@�;d@��@ʟ�@�M�@�hs@�I�@�v�@�M�@�"�@�~�@��@�x�@�1'@� �@�S�@§�@�n�@�-@���@��D@�"�@��+@���@��7@�G�@�%@��9@�j@��@�K�@�K�@���@��P@�;d@�+@��y@��+@�@���@�&�@��j@�A�@��;@��F@���@��P@���@�J@��@��h@�hs@�X@�G�@�7L@��@�I�@��@���@�t�@�C�@�"�@��!@�v�@�$�@��@�O�@��@��9@�j@� �@�1@��
@��@�n�@�{@���@�?}@���@��@��;@��
@��F@�dZ@��y@���@�=q@�`B@��/@�j@��@�l�@�C�@�"�@��@��\@�V@�J@���@���@�(�@��@��w@�l�@�;d@�+@�"�@��@��H@�M�@���@��h@�x�@�p�@�O�@�?}@���@��u@� �@�K�@�ȴ@�~�@�M�@��@��@��h@�G�@��@��j@�Z@��@���@�l�@��@��@���@�{@���@�O�@��@�Ĝ@�bN@�Z@�I�@� �@��@��@�ƨ@�\)@�+@��@�ff@���@�O�@�Ĝ@�j@�(�@�  @��m@���@��@�\)@�C�@�"�@���@���@�~�@�V@�-@�{@��#@��@�I�@���@v�y@m`B@f�@_�w@U`B@NV@E�-@=��@7\)@1%@+"�@&ff@ ��@z�@��@�!@O�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�VA�VA�S�A�K�A�G�A�E�A�G�A�G�A�G�A�G�A�E�A�E�A�?}A�1'A�7LA�9XA�9XA�5?A�7LA�7LA�9XA�9XA�9XA�;dA�9XA�9XA�;dA�;dA�=qA�=qA�?}A�=qA�9XA�7LA�5?A�/A�-A�/A�/A�/Aߟ�A�bNA�33A��A���A�;dA�~�A�
=A�7LA��Aɡ�A�$�A�Q�A���AƲ-AžwA�VA�JA��A���A��
A��A�33A��DA�"�A���A���A��A� �A���A���A�=qA� �A��A�5?A��A�jA���A�9XA�C�A��hA���A��A�n�A�M�A��A��uA��A�-A�ZA�ffA���A�r�A��/A�ZA��
A�ĜA�(�A�  A�ƨA���A��TA�mA{�-Aup�AqVAnE�Al��Aj��AgAd��AaA_oA[�AV��ATȴAT-ARȴAO��AMt�AK+AH�yAE�;ACoA@�/A>��A<ȴA;"�A8��A81A7�wA7dZA6�A3��A0�A-A+��A*9XA(�DA'"�A$�A$5?A#XA#/A!�FA ��A 5?A!+A"9XA$�RA%��A&��A&ĜA&(�A$��A#��A"��A"�\A!�A/AbNAZA��A�TA�-A�AoA7LA�\A�^A^5Av�A|�AZA�A��AXA�DA
�yA	��A	x�A�yA�\A�#A^5A��AhsA{A �HA �RA ��A �+A ��A33AƨAdZAG�AAdZA+A%A �RA M�@���@�^5@��@���@���@�M�@�dZ@�C�@�C�@�33@�|�@��P@�t�@�"�@��@�=q@��@���@�z�@�A�@��w@�C�@�ȴ@�{@��@�t�@���@�=q@�E�@��-@��9@�33@�n�@��#@�hs@�V@� �@�=q@�1'@���@��@�@��@陚@�?}@�hs@�9@�=q@�9@��@�^5@�`B@�O�@��@�Ĝ@�u@��;@��;@�@��y@�M�@�J@�I�@�=q@��@�\)@�^5@�hs@ؼj@�A�@ו�@��H@�ȴ@�ff@�7L@ԣ�@�ƨ@ӕ�@�dZ@�;d@�o@��y@��@�=q@��#@ѡ�@�x�@�@җ�@�ff@�
=@�o@���@��@��@Гu@�z�@�z�@�j@�Z@�1'@��m@�1@ϥ�@�@��H@�ȴ@�~�@��@�O�@�V@��@�Ĝ@�Z@��
@�;d@��@ʟ�@�M�@�hs@�I�@�v�@�M�@�"�@�~�@��@�x�@�1'@� �@�S�@§�@�n�@�-@���@��D@�"�@��+@���@��7@�G�@�%@��9@�j@��@�K�@�K�@���@��P@�;d@�+@��y@��+@�@���@�&�@��j@�A�@��;@��F@���@��P@���@�J@��@��h@�hs@�X@�G�@�7L@��@�I�@��@���@�t�@�C�@�"�@��!@�v�@�$�@��@�O�@��@��9@�j@� �@�1@��
@��@�n�@�{@���@�?}@���@��@��;@��
@��F@�dZ@��y@���@�=q@�`B@��/@�j@��@�l�@�C�@�"�@��@��\@�V@�J@���@���@�(�@��@��w@�l�@�;d@�+@�"�@��@��H@�M�@���@��h@�x�@�p�@�O�@�?}@���@��u@� �@�K�@�ȴ@�~�@�M�@��@��@��h@�G�@��@��j@�Z@��@���@�l�@��@��@���@�{@���@�O�@��@�Ĝ@�bN@�Z@�I�@� �@��@��@�ƨ@�\)@�+@��@�ff@���@�O�@�Ĝ@�j@�(�@�  @��m@���@��@�\)@�C�@�"�@���@���@�~�@�V@�-@�{G�O�@��@�I�@���@v�y@m`B@f�@_�w@U`B@NV@E�-@=��@7\)@1%@+"�@&ff@ ��@z�@��@�!@O�@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
@�B
@�B
@�B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
D�B
C�B
C�B
C�B
H�B
S�B
�B
��B
�BB;dBq�BB�BB�B��B/BjBr�B�VB�\B�PB�BɺB��BǮB��B��B�5B�B�B��BJBDBDB1B"�B&�BoB��B�fB�BɺB�?B��B��Bz�B`BBK�B�B�B�B�9B�B��B�+Bl�BS�BH�B0!BuB
��B
�uB
e`B
YB
2-B	�B	��B	��B	|�B	iyB	_;B	R�B	D�B	;dB	)�B	�B	+B��B�B�B�B��B�B�B�B�B�ZB�;B�)B�/B�yB�B�B�B�B�sB�
B��B��BǮBB��B�^BĜBĜB�}B�qB�FBÖBĜB�;B��B	/B	H�B	dZB	q�B	s�B	q�B	l�B	u�B	�B	}�B	`BB	-B	�B	uB	�B	 �B	�B	bB	B	B��B	JB	�B��B�fB�`B�B�B	%B	PB	B	  B��B��B�B�ZB�B��BɺBÖBÖBÖBŢB��B�
B�B��B	B	uB	�B	&�B	1'B	9XB	A�B	H�B	I�B	I�B	[#B	aHB	gmB	s�B	u�B	x�B	|�B	�B	�%B	�VB	�VB	�VB	�\B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�bB	�PB	�=B	�JB	�\B	�{B	��B	��B	��B	��B	�{B	�bB	�PB	�=B	�=B	�VB	��B	�B	�B	�B	�-B	�FB	�?B	�3B	�-B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�RB	�qB	�}B	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�
B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�
B	�
B	�B	�
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�#B	�;B	�HB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
	7B
	7B

=B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
\B
\B
hB
oB
oB
oB
oB
oB
uB
uB
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
-B
2-B
7LB
?}B
D�B
J�B
P�B
VB
[#B
bNB
e`B
k�B
o�B
s�B
w�B
|�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
@iB
@lB
@mB
?cB
?dB
?cB
?cB
@jB
@mB
@lB
@jB
@lB
@hB
AoB
@lB
@lB
@lB
AqB
AoB
AqB
AsB
AoB
AqB
AoB
AsB
AqB
AsB
AqB
AqB
AqB
AqB
BxB
BxB
CB
CB
C{B
D�B
CB
C}B
CB
H�B
S�B
��B
˩B
�B�B;ABq�B�pB� B�B��B.�Bj_Br�B�5B�<B�2B��BəB˥BǌBʞBζB�B�_B�B��B(B"B#BB"�B&�BOB��B�DB��BɗB�B��B��Bz�B`BK�B�B�~B��B�B��B�B�BlfBS�BH�B/�BQB
ϺB
�QB
e@B
X�B
2B	�\B	�dB	�aB	|�B	iZB	_B	R�B	D�B	;HB	)�B	�B	B��B�B�B��B��B�B�nB�uB�dB�?B� B�B�B�]B�sB�oB�nB�hB�VB��B��BλBǐB�rB�fB�ABĂBăB�`B�TB�*B�wB�~B�B��B	.�B	H�B	d7B	q�B	s�B	q�B	lfB	u�B	��B	}�B	` B	,�B	gB	RB	�B	 �B	B	?B	�B	 �B��B	*B	^B��B�EB�BB�wB�B	B	.B	�B��B��B��B�uB�:B��B��BəB�tB�vB�wBŁB̭B��B�bB��B	�B	QB	�B	&�B	1B	93B	AdB	H�B	I�B	I�B	Z�B	a$B	gFB	s�B	u�B	x�B	|�B	��B	��B	�,B	�-B	�.B	�3B	�<B	�eB	�pB	�}B	��B	��B	�}B	�rB	�eB	�aB	�_B	�lB	�}B	�}B	��B	�}B	�}B	�~B	�qB	�iB	�SB	�=B	�*B	�B	� B	�1B	�UB	�dB	�sB	��B	�|B	�RB	�7B	�'B	�B	�B	�/B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�%B	�FB	�OB	�{B	ɑB	έB	̢B	ʗB	ʓB	˚B	̡B	̡B	ͧB	ήB	ͧB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ѿB	ϲB	˚B	ήB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ѿB	ѿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�-B	�1B	�4B	�;B	�7B	�9B	�?B	�AB	�@B	�GB	�MB	�JB	�aB	�eB	�eB	�eB	�eB	�eB	�fB	�_B	�^B	�\B	�^B	�]B	�_B	�^B	�XB	�WB	�PB	�QB	�VB	�WB	�WB	�\B	�eB	�kB	�iB	�jB	�pB	�pB	�xB	�qB	�oB	�kB	�cB	�hB	�hB	�lB	�oB	�kB	�oB	�rB	�cB	�eB	�dB	�fB	�jB	�oB	�nB	�oB	�pB	�rB	�vB	�uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
	B
	B

B

B

B
B
B
B
B
B
B
B
!B
,B
-B
8B
?B
=B
>B
<B
=B
FB
EB
JB
FB
IB
LB
RB
PB
RB
PG�O�B
cB
iB
�B
%�B
,�B
1�B
7B
?LB
DkB
J�B
P�B
U�B
Z�B
bB
e0B
kSB
okB
s�B
w�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.41 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451232016080714512320160807145123  AO  ARCAADJP                                                                    20151110031604    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151110031604  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151110031604  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145123  IP                  G�O�G�O�G�O�                