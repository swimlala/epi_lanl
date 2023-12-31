CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-17T09:15:32Z AOML 3.0 creation; 2016-06-01T00:08:25Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150617091532  20160531170825  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               vA   AO  4055_7112_118                   2C  D   APEX                            5374                            041511                          846 @�Y�� 1   @�Y	d� @;}�-V�d@I�^5?1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    vA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP�CR�CT�CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyffD��D�Y�D��3D��fD�  D�<�D�y�D���D�	�D�L�D�ffD�ɚD��D�33Dڃ3D�ɚD�	�D�FfD�vfD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��A�\A&�\AF�\Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C h�Ch�Ch�Ch�Ch�C
h�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C h�C"h�C$h�C&O\C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CN��CP��CR��CT��CVh�CXh�CZh�C\h�C^h�C`h�Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%=D%�=D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9��D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt�pDy��D��D�f�D��RD���D�D�I�D���D���D��D�Y�D�s�D�ֹD�)�D�@RDڐRD�ֹD��D�S�D�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A���A�E�A���A�+A�jA�$�A��#A��!A��+A�9XA�A���A�(�A�JA�ȴA�{A��A�I�A�A�I�A��PA��7A�hsA��;A�/A���A���A���A�dZA��A�\)A���A���A��A�\)A�1A�=qA�Q�A��A��A�7LA���A���A�S�A��#A�A��A�I�A���A��#A�A�A��!A��A�C�A��7A��-A���A���A�+A��
A�l�A�$�A���A�1A���A���A�;dA�ĜA��DA�C�A���A��RA�ffA�Q�A� �A�C�A�ȴA���A��A�A�A���A��jA���A��PA�A�A���A���A��7A�n�A�$�A�
=A��A��-A�\)A�+A�{A��;A���A���A�l�A�E�A�{A���A�p�A�I�A���A��!A�\)A��A�A��A�\)A�+A�ȴA�p�A���A��#A�p�A���A�  A�M�A��A�\)A��A�hsA��A�#A�A&�A~1'A|n�Az�AyVAw�^Av�yAv-At��As\)Ar��Ar9XAq�Ao�An-Al�RAlA�AkdZAj�DAhĜAgx�Ad��Ab�Aa��Aa\)A`�A`5?A_/A^�A\��A[G�A[
=AY�TAXAW%AVjAV�AUAUoAS;dAR�!ARAQ
=AP�9API�AN�AM�
ALJAK�^AJE�AH1AF�AE��ADffABM�A@�A>Q�A8��A5��A5
=A3��A3/A3"�A3�A3�A3%A2�!A1`BA0�uA0ZA.��A,ĜA+|�A+oA*�A*5?A*(�A)�wA)%A(ffA't�A&�`A&E�A&�A%�;A%XA$��A$^5A#�TA#�7A"��A!�A �yA 5?A5?A��A;dA�9A$�AO�A�!AI�A�A�#A�-AdZAĜA5?A�AXA�AbNA��AoA�FAA��Ar�A��At�AdZA-A�;A��A�PA�AXA
��A	�;A	+AVA��A?}A�HA�#A�/A~�A(�A�-AoA�DAQ�AbA�A �HA  �A �@�  @���@��@���@���@�9X@��m@�P@�+@�!@�ff@�?}@�1@��y@��@�o@噚@� �@�@���@�/@�l�@ޟ�@އ+@�^5@�O�@ۥ�@�A�@�@��m@Л�@��@�/@�I�@�ƨ@��@Ȭ@�X@î@°!@���@�I�@�K�@��@��@�dZ@�ff@��^@�hs@��@���@���@� �@���@�33@��+@�E�@��-@�V@�Q�@��@�v�@��#@���@�hs@���@�Q�@��@��F@�S�@�^5@���@���@�  @���@�V@�{@��#@���@�hs@�O�@�&�@��/@�+@���@��D@�1'@��@�K�@��H@�n�@�5?@���@�@�O�@�Ĝ@�A�@��P@��@��y@���@�ff@��T@�%@��@� �@�ƨ@�C�@�
=@��H@��@���@�M�@�M�@�M�@�E�@�@�p�@��@��@���@���@�S�@��!@���@�&�@��@�+@���@���@��@��`@��u@��F@�=q@��T@��-@�p�@�G�@�7L@��@���@�Z@�1'@�b@���@��
@�l�@��H@���@���@�n�@��@�@��#@���@�7L@���@�bN@��m@�+@��y@��y@�ȴ@��!@�n�@�5?@��^@��`@���@�z�@�9X@��w@���@�l�@�ȴ@�E�@���@��h@�x�@�x�@�p�@��/@�Q�@�  @�;@�w@��@�P@��@�@�@
=@~@|Z@{�F@{@z�H@z^5@y�@yG�@xĜ@x�@xb@w\)@v$�@u�@t�@t��@tZ@t9X@t�@rJ@h�`@a�^@W��@Q%@KdZ@EO�@AG�@;@6ȴ@/�@(1'@#�F@ A�@-@+@I�@��@��@
�\@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�n�A���A�E�A���A�+A�jA�$�A��#A��!A��+A�9XA�A���A�(�A�JA�ȴA�{A��A�I�A�A�I�A��PA��7A�hsA��;A�/A���A���A���A�dZA��A�\)A���A���A��A�\)A�1A�=qA�Q�A��A��A�7LA���A���A�S�A��#A�A��A�I�A���A��#A�A�A��!A��A�C�A��7A��-A���A���A�+A��
A�l�A�$�A���A�1A���A���A�;dA�ĜA��DA�C�A���A��RA�ffA�Q�A� �A�C�A�ȴA���A��A�A�A���A��jA���A��PA�A�A���A���A��7A�n�A�$�A�
=A��A��-A�\)A�+A�{A��;A���A���A�l�A�E�A�{A���A�p�A�I�A���A��!A�\)A��A�A��A�\)A�+A�ȴA�p�A���A��#A�p�A���A�  A�M�A��A�\)A��A�hsA��A�#A�A&�A~1'A|n�Az�AyVAw�^Av�yAv-At��As\)Ar��Ar9XAq�Ao�An-Al�RAlA�AkdZAj�DAhĜAgx�Ad��Ab�Aa��Aa\)A`�A`5?A_/A^�A\��A[G�A[
=AY�TAXAW%AVjAV�AUAUoAS;dAR�!ARAQ
=AP�9API�AN�AM�
ALJAK�^AJE�AH1AF�AE��ADffABM�A@�A>Q�A8��A5��A5
=A3��A3/A3"�A3�A3�A3%A2�!A1`BA0�uA0ZA.��A,ĜA+|�A+oA*�A*5?A*(�A)�wA)%A(ffA't�A&�`A&E�A&�A%�;A%XA$��A$^5A#�TA#�7A"��A!�A �yA 5?A5?A��A;dA�9A$�AO�A�!AI�A�A�#A�-AdZAĜA5?A�AXA�AbNA��AoA�FAA��Ar�A��At�AdZA-A�;A��A�PA�AXA
��A	�;A	+AVA��A?}A�HA�#A�/A~�A(�A�-AoA�DAQ�AbA�A �HA  �A �@�  @���@��@���@���@�9X@��m@�P@�+@�!@�ff@�?}@�1@��y@��@�o@噚@� �@�@���@�/@�l�@ޟ�@އ+@�^5@�O�@ۥ�@�A�@�@��m@Л�@��@�/@�I�@�ƨ@��@Ȭ@�X@î@°!@���@�I�@�K�@��@��@�dZ@�ff@��^@�hs@��@���@���@� �@���@�33@��+@�E�@��-@�V@�Q�@��@�v�@��#@���@�hs@���@�Q�@��@��F@�S�@�^5@���@���@�  @���@�V@�{@��#@���@�hs@�O�@�&�@��/@�+@���@��D@�1'@��@�K�@��H@�n�@�5?@���@�@�O�@�Ĝ@�A�@��P@��@��y@���@�ff@��T@�%@��@� �@�ƨ@�C�@�
=@��H@��@���@�M�@�M�@�M�@�E�@�@�p�@��@��@���@���@�S�@��!@���@�&�@��@�+@���@���@��@��`@��u@��F@�=q@��T@��-@�p�@�G�@�7L@��@���@�Z@�1'@�b@���@��
@�l�@��H@���@���@�n�@��@�@��#@���@�7L@���@�bN@��m@�+@��y@��y@�ȴ@��!@�n�@�5?@��^@��`@���@�z�@�9X@��w@���@�l�@�ȴ@�E�@���@��h@�x�@�x�@�p�@��/@�Q�@�  @�;@�w@��@�P@��@�@�@
=@~@|Z@{�F@{@z�H@z^5@y�@yG�@xĜ@x�@xb@w\)@v$�@u�@t�@t��@tZ@t9X@t�@rJ@h�`@a�^@W��@Q%@KdZ@EO�@AG�@;@6ȴ@/�@(1'@#�F@ A�@-@+@I�@��@��@
�\@V11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B�B�'B�3B�9B�3B�-B�-B�'B�!B�B��B��B��B�PBv�BiyBe`BaHB\)BbNBffBe`BdZBaHBdZBaHB_;B_;B]/B]/B\)BZBXBS�BM�BG�BB�B@�BA�BJ�BK�BD�B<jB7LB33B'�B�BbB%B��B�fB�BŢB�9B��B��B�VB�%B� Bu�Bk�BffBcTB[#BQ�BK�B9XB0!B-B%�BuBB��B�B�B�B�B�B�mB�ZB�HB�)B��B��B��B��B��B��BȴBÖB�dB�FB�9B�!B�B�B��B��B��B��B��B�uB�JB�B}�Bt�Bp�BjBffBcTB\)BT�BI�BE�B<jB+B�BDBB
��B
�B
�HB
�#B
�B
��B
��B
ŢB
�RB
��B
�uB
�B
x�B
o�B
`BB
P�B
I�B
B�B
9XB
,B
"�B
�B
\B
%B	��B	�B	�/B	ȴB	�}B	�dB	�RB	�?B	�!B	��B	��B	��B	�oB	�\B	�1B	�B	}�B	z�B	x�B	v�B	r�B	hsB	dZB	`BB	ZB	W
B	R�B	L�B	G�B	?}B	<jB	5?B	.B	+B	(�B	$�B	�B	1B��B�5B��B��B��B��B��B��B��B��BɺBŢBĜB��B�jB�RB�?B�3B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�JB�1B� Bz�By�By�Bx�Bw�Bv�Bu�Bt�Bt�Bs�Br�Bp�Bn�Bm�Bl�BiyBffBcTBaHB\)BZBXBW
BT�BP�BL�BK�BK�BJ�BJ�BI�BH�BF�BD�BC�BB�BA�B@�B>wB<jB:^B9XB8RB6FB5?B49B33B2-B0!B.B-B)�B#�B!�B �B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B{BuBuBuB{BuBuBoBhBoBuBuB�B�B�B�B�B�B�B�B �B!�B"�B$�B&�B'�B,B/B2-B33B33B49B49B49B5?B6FB7LB9XB9XB;dB=qB>wBC�BE�BG�BG�BG�BH�BI�BI�BJ�BJ�BN�BQ�BT�BXB]/B^5B_;B_;B_;B`BB`BB`BB_;BffBiyBk�Bk�Bk�Bl�Bl�Bm�Bm�Bn�Bo�Bq�Bs�Bt�Bx�Bz�B{�B|�B|�B� B�B�+B�+B�7B�DB�PB�VB�\B�oB��B��B��B��B��B��B��B��B��B�B�B�-B�FB�RB�qB��BBǮB��B��B��B��B�5B�BB�NB�ZB�`B�`B�fB�mB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	+B		7B	JB	PB	\B	hB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	$�B	&�B	'�B	(�B	(�B	+B	,B	,B	/B	49B	5?B	6FB	6FB	7LB	8RB	9XB	:^B	=qB	>wB	B�B	D�B	G�B	H�B	J�B	L�B	N�B	N�B	P�B	R�B	S�B	XB	[#B	^5B	aHB	bNB	cTB	dZB	m�B	��B	�!B	��B	�HB	�B	��B
	7B
�B
!�B
1'B
<jB
C�B
H�B
R�B
XB
\)B
cTB
gmB
jB
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�oB��B��B��B��B�B�B�B�B�B�B�	B�B��B��B��B��B�/Bv�BiXBe=Ba*B\	Bb+BfDBe=Bd8Ba'Bd:Ba(B_B_B]B]B\BY�BW�BS�BM�BG�BBkB@^BAeBJ�BK�BD{B<GB7,B3B'�BtB<B�B��B�CB��B�{B�B��B�ZB�1B��B�Bu�Bk^Bf=Bc/BZ�BQ�BK�B9.B/�B,�B%�BMB �B��B�zB�|B�B�uB�gB�CB�.B� B��B��B��B��B��BͫB˜BȋB�jB�=B�B�B��B��B��B��B��B��B��B�cB�MB�"B��B}�Bt�Bp~BjTBf>Bc-B\ BT�BI�BExB<AB*�B{BB �B
��B
�jB
�#B
��B
��B
��B
βB
�zB
�+B
��B
�QB
��B
x�B
oyB
`B
P�B
I�B
BnB
93B
+�B
"�B
\B
:B
B	��B	�eB	�B	ȑB	�ZB	�CB	�0B	�B	� B	��B	��B	�sB	�PB	�<B	�B	��B	}�B	z�B	x�B	v�B	r�B	hVB	d:B	`%B	Y�B	V�B	R�B	L�B	G�B	?`B	<LB	5!B	-�B	*�B	(�B	$�B	vB	B��B�B��B��B��BνB͸B͸B̲B˫BɝBňBăB�nB�PB�8B�$B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�mB�aB�IB�0B�B�Bz�By�By�Bx�Bw�Bv�Bu�Bt�Bt�Bs�Br�Bp�BnBmvBlsBi_BfMBc;Ba0B\BZBW�BV�BT�BP�BL�BK�BK�BJ�BJ�BI�BH�BF�BD�BC}BBvBApB@iB>^B<SB:FB9%B89B6.B5'B4"B3B1�B0B-�B,�B)�B#�B!�B �B �B �B �B�B�B�B�B�B�BsBvBSBjBgBiBcB\B[B[BbB\B[BXB5BVB[B[BgByBBkB�B�BlB�B �B!�B"�B$�B&�B'�B+�B/ B2B3B3B4B4B4B5%B6&B70B9=B9:B;HB=TB>[BCxBE�BG�BG�BG�BH�BI�BI�BJ�BJ�BN�BQ�BT�BW�B]B^B_B_B_B`"B`#B`"B_BfIBiZBkeBkfBkfBlkBljBmqBmsBnwBo�Bq�Bs�Bt�Bx�Bz�B{�B|�B|�B�B��B�B�	B�B�%B�.B�5B�:B�NB�cB�rB�yB��B��B��B��B��B��B��B��B�
B�!B�/B�LB�_B�jBǊBˢB̨BγB��B�B�B�)B�4B�7B�:B�CB�EB�TB�WB�XB�YB�XB�cB�|B�B�B��B��B��B��B��B��B	�B	�B	B		B	 B	)B	5B	AB	XB	\B	]B	uB	sB	pB	|B	�B	 �B	 �B	$�B	&�B	'�B	(�B	(�B	*�B	+�B	+�B	.�B	4B	5B	6B	6B	7!B	8&B	9/B	:5B	=HB	>MB	BcB	DsB	G�B	H�B	J�B	L�B	N�B	N�B	P�B	R�B	S�B	W�B	Z�B	^	B	aB	b"B	c)B	d1B	mdB	�fB	��B	έB	�B	�sB	��B
	B
SB
!�B
0�B
<9B
CeB
H�B
R�B
W�B
[�B
c B
g<B
jNB
qx11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.41 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708252016053117082520160531170825  AO  ARCAADJP                                                                    20150617091532    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150617091532  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150617091532  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170825  IP                  G�O�G�O�G�O�                