CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:14:27Z AOML 3.0 creation; 2016-08-07T21:51:13Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221427  20160807145113  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5287_9017_025                   2C  D   APEX                            6529                            072314                          846 @�-�% 1   @�-ff�@21&�y�dXQ��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A   A   AA��Aa��A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|  C~  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�3D��D�L�D�|�D��3D�3D�VfD�|�D��3D�3D�C3D���D���D��3D�L�Dډ�D��fD��D�<�D�p D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A�\A&�\AH(�Ah(�A�{A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B���B�B�8RB���C h�Ch�Ch�Ch�Ch�C
h�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C h�C"h�C$h�C&h�C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^h�C`h�Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Cz��C|h�C~h�C�4{C�'�C�'�C�'�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D��D�D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%=D%�=D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt�=DupDy�pD��D�Y�D���D��RD�RD�c�D���D��RD�RD�PRD���D���D� RD�Y�Dږ�D�ӅD��D�I�D�}D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A� �A�&�A�9XA�A�A�A�A�=qA�/A�-A�-A�(�A�$�A�$�A�"�A�"�A�$�A�(�A�/A�-A�+A�/A�7LA�7LA�;dA�;dA�K�A�VA�p�A�t�A�z�A�bNA�bNA�`BA�\)A�jAЏ\AЉ7AЇ+AЛ�AЛ�AН�AХ�AЮA�ĜAоwA�ȴA��#A���A�{A�9XA�S�A�l�AхAљ�Aї�A�n�A�;dA�AжFA�(�A�?}A�5?Aƙ�A�$�A�
=A�l�A���A��yA�XA�p�A�33A��A���A��HA�ZA���A��wA��A�K�A���A��7A���A���A���A��A���A��yA�A�1'A���A�  A�`BA�A�A���A�?}A�|�A�K�A��9A�G�A�ȴA��A�ffA�x�A��A�$�A�x�A�^5A�Q�A�ZA�v�A�K�A�
=A�ZA���A}�A|��A{�mAy7LApJAl1Aj��AjAghsA`�A]�
AVĜAT5?AO��AM��AL�`AL�\ALJAK�hAI�AH�uAE��AC33AA�#A?��A=��A<r�A;�A;dZA:  A7��A6~�A5��A4��A41'A3oA1�PA0E�A.��A-\)A,��A,��A,r�A+�TA*�!A&��A#/A"^5A!��A ��A  �A/AE�AC�A;dA�
A�;A��A�A�AdZA��Ap�A�A�+AAS�A
=A��AbA�-AhsA�yA��AdZAG�A�AȴA�^AO�A7LA
r�A	�A	�A�!AE�A�;A`BA��A9XAhsA�yAffA=qA�A�-A`BA;dA%A�jA�AE�A�@��@���@���@��
@���@��H@�J@��D@�\)@�o@�;d@�;d@��@��\@���@��@��@�|�@�!@�X@��/@�@�(�@�F@�{@��T@���@��`@�A�@�F@�S�@��@��y@�~�@�=q@�x�@�S�@�M�@��#@�V@�K�@�^@��D@�  @ߥ�@߅@�+@�@���@�x�@��@�bN@ۮ@�l�@�o@�^5@ٲ-@�O�@�&�@���@���@�K�@���@�V@��@Ձ@�O�@�?}@�Ĝ@ԓu@�j@�(�@�ƨ@Ӿw@ӥ�@�dZ@�K�@��y@��@�r�@�Z@�A�@�  @ϥ�@�dZ@�\)@�K�@�
=@���@Ώ\@�5?@Ͳ-@͡�@�`B@̼j@�  @˥�@�;d@ʗ�@�M�@�-@ɲ-@�x�@�p�@�`B@�7L@�V@���@�1'@�ƨ@��@�ff@�5?@��@Ł@�V@�  @��;@î@Å@�dZ@�
=@§�@�~�@�$�@���@�/@���@���@�+@��R@�ff@�=q@���@�O�@�Ĝ@���@�9X@�ƨ@�l�@��@�$�@�?}@��`@��@�t�@��R@�^5@�$�@�{@��@��h@��@�z�@�b@��
@�dZ@�
=@���@��!@�ff@�$�@���@�hs@�G�@��@��@��@��;@�S�@�;d@�
=@��!@�~�@�-@���@�G�@�Ĝ@���@��@�j@�bN@�Z@�A�@�(�@�ƨ@���@�\)@���@�=q@���@�7L@��@��@��@���@��D@�b@��@���@��
@�t�@�33@��H@�=q@��@�O�@�r�@�  @��
@��
@���@��P@�"�@��#@��/@�j@�1@��
@��F@��P@��@��@��R@�=q@�p�@��@�r�@�A�@�  @���@�K�@��R@��@��7@�X@�?}@�/@���@���@��@�z�@�Q�@�1'@��w@��@�l�@�o@��!@�v�@�-@���@��T@��#@��^@��7@�V@���@�j@�Q�@�A�@���@��
@��@�;d@�
=@���@��H@��@���@�M�@�$�@�1@�v�@�b@��@x�@l��@d�@Z^5@O|�@HĜ@@b@9&�@1��@,j@';d@$1@ ��@�-@Q�@�F@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A� �A�&�A�9XA�A�A�A�A�=qA�/A�-A�-A�(�A�$�A�$�A�"�A�"�A�$�A�(�A�/A�-A�+A�/A�7LA�7LA�;dA�;dA�K�A�VA�p�A�t�A�z�A�bNA�bNA�`BA�\)A�jAЏ\AЉ7AЇ+AЛ�AЛ�AН�AХ�AЮA�ĜAоwA�ȴA��#A���A�{A�9XA�S�A�l�AхAљ�Aї�A�n�A�;dA�AжFA�(�A�?}A�5?Aƙ�A�$�A�
=A�l�A���A��yA�XA�p�A�33A��A���A��HA�ZA���A��wA��A�K�A���A��7A���A���A���A��A���A��yA�A�1'A���A�  A�`BA�A�A���A�?}A�|�A�K�A��9A�G�A�ȴA��A�ffA�x�A��A�$�A�x�A�^5A�Q�A�ZA�v�A�K�A�
=A�ZA���A}�A|��A{�mAy7LApJAl1Aj��AjAghsA`�A]�
AVĜAT5?AO��AM��AL�`AL�\ALJAK�hAI�AH�uAE��AC33AA�#A?��A=��A<r�A;�A;dZA:  A7��A6~�A5��A4��A41'A3oA1�PA0E�A.��A-\)A,��A,��A,r�A+�TA*�!A&��A#/A"^5A!��A ��A  �A/AE�AC�A;dA�
A�;A��A�A�AdZA��Ap�A�A�+AAS�A
=A��AbA�-AhsA�yA��AdZAG�A�AȴA�^AO�A7LA
r�A	�A	�A�!AE�A�;A`BA��A9XAhsA�yAffA=qA�A�-A`BA;dA%A�jA�AE�A�@��@���@���@��
@���@��H@�J@��D@�\)@�o@�;d@�;d@��@��\@���@��@��@�|�@�!@�X@��/@�@�(�@�F@�{@��T@���@��`@�A�@�F@�S�@��@��y@�~�@�=q@�x�@�S�@�M�@��#@�V@�K�@�^@��D@�  @ߥ�@߅@�+@�@���@�x�@��@�bN@ۮ@�l�@�o@�^5@ٲ-@�O�@�&�@���@���@�K�@���@�V@��@Ձ@�O�@�?}@�Ĝ@ԓu@�j@�(�@�ƨ@Ӿw@ӥ�@�dZ@�K�@��y@��@�r�@�Z@�A�@�  @ϥ�@�dZ@�\)@�K�@�
=@���@Ώ\@�5?@Ͳ-@͡�@�`B@̼j@�  @˥�@�;d@ʗ�@�M�@�-@ɲ-@�x�@�p�@�`B@�7L@�V@���@�1'@�ƨ@��@�ff@�5?@��@Ł@�V@�  @��;@î@Å@�dZ@�
=@§�@�~�@�$�@���@�/@���@���@�+@��R@�ff@�=q@���@�O�@�Ĝ@���@�9X@�ƨ@�l�@��@�$�@�?}@��`@��@�t�@��R@�^5@�$�@�{@��@��h@��@�z�@�b@��
@�dZ@�
=@���@��!@�ff@�$�@���@�hs@�G�@��@��@��@��;@�S�@�;d@�
=@��!@�~�@�-@���@�G�@�Ĝ@���@��@�j@�bN@�Z@�A�@�(�@�ƨ@���@�\)@���@�=q@���@�7L@��@��@��@���@��D@�b@��@���@��
@�t�@�33@��H@�=q@��@�O�@�r�@�  @��
@��
@���@��P@�"�@��#@��/@�j@�1@��
@��F@��P@��@��@��R@�=q@�p�@��@�r�@�A�@�  @���@�K�@��R@��@��7@�X@�?}@�/@���@���@��@�z�@�Q�@�1'@��w@��@�l�@�o@��!@�v�@�-@���@��T@��#@��^@��7@�V@���@�j@�Q�@�A�@���@��
@��@�;d@�
=@���@��H@��@���@�M�G�O�@�1@�v�@�b@��@x�@l��@d�@Z^5@O|�@HĜ@@b@9&�@1��@,j@';d@$1@ ��@�-@Q�@�F@�9111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB6FB6FB7LB8RB8RB7LB5?B5?B5?B49B49B49B5?B5?B49B33B33B33B33B33B33B33B49B49B7LB:^BA�BA�BB�B;dB;dB;dB:^B>wBH�BF�BE�BJ�BJ�BJ�BM�BP�BXBT�BXB^5BgmBo�By�B�B�=B�uB��B�B�FB�XB�^B�^B�jB��B�B�sB
=B7LBVBn�Bn�Bz�Bl�BhsBm�Bq�B� B�=B�+By�Bl�BdZB`BB[#BS�BL�BD�B:^B33B'�BhB+B  B��B�5B�)B�B��B�B�PBz�Br�Bl�BcTBJ�B!�BB
��B
�`B
ǮB
��B
�\B
gmB
T�B
P�B
I�B
;dB
�B	��B	��B	�B	��B	��B	�B	x�B	r�B	bNB	=qB	)�B	JB��B��B�B�B�B�sB�`B�NB�/B�B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�/B�5B�5B�5B�/B�)B�#B�B�B�B�B�
B�B�B�B�B�B�;B�5B�BB�HB�BB�BB�ZB�yB�B��B	  B	+B	DB	\B	hB	uB	�B	 �B	#�B	#�B	&�B	+B	.B	5?B	=qB	H�B	J�B	O�B	Q�B	R�B	T�B	T�B	T�B	S�B	S�B	T�B	[#B	^5B	`BB	dZB	cTB	cTB	cTB	bNB	bNB	aHB	^5B	ZB	YB	XB	XB	XB	XB	W
B	XB	XB	[#B	^5B	_;B	`BB	`BB	aHB	e`B	ffB	ffB	jB	k�B	l�B	p�B	q�B	v�B	}�B	� B	�B	}�B	}�B	�B	�B	�B	�B	�B	�B	�B	�1B	�1B	�1B	�7B	�JB	�PB	�\B	�bB	�hB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�3B	�9B	�9B	�?B	�FB	�FB	�LB	�RB	�RB	�RB	�XB	�XB	�XB	�}B	B	B	ÖB	ƨB	ǮB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�5B	�;B	�BB	�BB	�BB	�;B	�;B	�5B	�5B	�/B	�;B	�;B	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�`B	�ZB	�TB	�NB	�BB	�;B	�;B	�;B	�BB	�BB	�;B	�;B	�;B	�BB	�HB	�HB	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�fB	�`B	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
1B
	7B
	7B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
	7B
	7B
	7B

=B
DB
hB
�B
�B
'�B
(�B
9XB
;dB
?}B
D�B
K�B
R�B
YB
^5B
ffB
iyB
k�B
n�B
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B6$B6$B7)B84B82B7+B5B5B5B4B4B4B5B5B4B3B3B3B3B3B3B3B4B4B7+B:>BAhBAgBBpB;DB;CB;CB:>B>WBH�BF�BEBJ�BJ�BJ�BM�BP�BW�BT�BW�B^BgLBo|By�B��B�B�SB��B��B�'B�6B�?B�?B�HB��B��B�RB
B7*BU�BnrBnuBz�BljBhRBmnBq�B�B�B�By�BlcBd7B`B[BS�BL�BDyB:6B3B'�BBBB��B��B�B�B��B�bB��B�(Bz�Br�BlgBc.BJ�B!�B�B
��B
�<B
ǈB
��B
�9B
gHB
T�B
P�B
I�B
;>B
xB	��B	��B	�ZB	��B	��B	��B	x�B	r�B	b0B	=UB	)�B	-B��B��B�}B�jB�cB�VB�EB�3B�B�B��B��B��B��BμB̲B˫BʩB̰B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B��B�B��B�B��B��B��B��B��B�B�B�B�%B�*B�"B�%B�;B�[B�nB��B��B		B	"B	:B	FB	SB	hB	 �B	#�B	#�B	&�B	*�B	-�B	5B	=LB	H�B	J�B	O�B	Q�B	R�B	T�B	T�B	T�B	S�B	S�B	T�B	Z�B	^B	`B	d4B	c.B	c-B	c-B	b(B	b(B	a$B	^B	Y�B	X�B	W�B	W�B	W�B	W�B	V�B	W�B	W�B	Z�B	^B	_B	`B	`B	a B	e9B	f>B	fAB	jYB	k_B	ldB	p|B	q�B	v�B	}�B	�B	��B	}�B	}�B	��B	��B	��B	��B	��B	��B	��B	�	B	�
B	�B	�B	� B	�'B	�4B	�8B	�@B	�@B	�?B	�EB	�]B	�cB	�kB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�B	�B	�B	�B	�B	�!B	�(B	�&B	�'B	�-B	�-B	�-B	�SB	�dB	�cB	�jB	�|B	ǅB	ȆB	ȇB	ȆB	ʘB	˚B	̡B	ͦB	ϴB	ϴB	йB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�"B	�B	� B	�"B	�!B	�'B	�)B	�-B	�.B	�4B	�2B	�3B	�9B	�8B	�:B	�2B	�-B	�)B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�&B	�'B	�'B	�&B	�B	� B	�&B	�*B	�-B	�+B	�+B	�+B	�.B	�4B	�AB	�9B	�5B	�.B	�-B	�1B	�3B	�3B	�4B	�4B	�4B	�4B	�9B	�@B	�@B	�@B	�HB	�LB	�KB	�QB	�XB	�OB	�OB	�]B	�]B	�^B	�`B	�cB	�fB	�dB	�qB	�}B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	B
		B
B
	B
	B
	
B
	B
	B
	B

B
	B
		B
	G�O�B
B
5B
^B
�B
'�B
(�B
9&B
;5B
?LB
DlB
K�B
R�B
X�B
^B
f4B
iGB
kTB
nfB
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.41 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451132016080714511320160807145113  AO  ARCAADJP                                                                    20150226221427    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221427  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221427  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145113  IP                  G�O�G�O�G�O�                