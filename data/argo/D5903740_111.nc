CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-06T19:16:34Z AOML 3.0 creation; 2016-06-01T00:08:24Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150406191634  20160531170824  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               oA   AO  4055_7112_111                   2C  D   APEX                            5374                            041511                          846 @�G �� 1   @�G!L��@:|j~��#�d97KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    oA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dts3DyffD� D�P D���D��3D�	�D�0 D���D���D� D�S3D�S3D��fD��D�9�D�s3D�ٚD���D�#3D� D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��A�\A&�\AF�\Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C h�Ch�Ch�Ch�Ch�C
h�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C h�C"h�C$h�C&h�C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^h�C`h�Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%=D%�=D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB��DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds��Dt=Dt�pDy��D�D�]D���D��RD��D�=D���D���D�D�`RD�`RD��D��D�F�DڀRD��D��D�0RD�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�1A��A��mA��A���A�~�A�bNA�(�A�  A��!A�dZA�$�A��
A�x�A�(�A��A�ĜA��A��wA���A��9A��A�+A�M�A���A�=qA��-A��A���A��A���A�A�A�ĜA��;A�dZA�-A�
=A��A��wA��A��uA�r�A��A���A��A�VA�(�A��TA��7A��DA���A���A�K�A��/A��DA�l�A�S�A�A��hA�ĜA�33A��A��uA��A�JA��hA�33A��A�=qA�oA�wA�A~=qA{�Ay�;Ax�DAw�AwK�Aw+Av��Au|�As�^ArM�Aq��Aqt�Ap�!AnĜAj�\AghsAf�9AfZAf  Ae?}Ad��AbĜA`��A` �A_XA^�\A]A]�A\�uA[�AZ��AY�TAX�AV�AU��AU/AU%AT�yAT�+ASdZAR1'AQ��AP=qAO+AN�HAN(�AK�TAJ��AI�7AH��AH  AF�AC�AB��AB�AAG�A?�A?�A=��A<z�A< �A;A:��A9t�A8�+A7�FA6�RA4��A3��A333A2ȴA2bNA1��A1&�A0  A.jA,�yA,A*v�A)dZA(��A'l�A'G�A'C�A'C�A&��A&�+A&I�A%�FA%dZA%7LA$�9A#�FA"�!A!G�A�
A��A�A��A%A��Ar�A��A�A+AA��A�A��AȴA��A5?A��A��A^5A�#AXA��AE�A�FAO�AjAp�A�A�AVA�A
E�A	�FA	�A	7LA�HA(�AS�A%AȴA�A �A�mA��A�uAjAVA1'A�PA�@�o@��H@�1@�S�@��h@�S�@��@�Q�@��
@�@�{@�7L@�Z@���@��@�@��@��@��@�7@�9@�9X@�K�@ߥ�@�A�@�V@�%@؃@�1'@׾w@�
=@ԣ�@�ff@���@Ϯ@�@�dZ@���@��@�5?@��#@ŉ7@�V@Ĵ9@ēu@�1@�@�b@�\)@��@�V@�J@���@���@�I�@��@��R@�5?@�Ĝ@�b@��F@�K�@���@�5?@�7L@�r�@��@���@�%@��@���@�&�@��9@��@��@�t�@�+@��R@���@�O�@��@���@��@�I�@�1'@�b@��@��;@�ƨ@��P@��@��@���@���@�v�@�M�@�=q@��@���@�hs@�%@��u@�j@�A�@�b@�33@���@��T@�&�@��@�ƨ@��@��@���@�^5@�M�@�=q@�$�@�{@���@���@�hs@��@��@�z�@���@�C�@���@���@��+@��+@�^5@��#@�7L@���@��@��@�^5@�M�@�-@�J@�J@��T@�`B@�Ĝ@�bN@�9X@�1'@� �@��@�b@��@�ƨ@�ƨ@��F@�33@�-@��^@�G�@��@��`@��@�(�@��@�ƨ@�+@�~�@�ff@�=q@���@�G�@��@��`@���@���@��u@�r�@�j@�bN@�Z@�(�@��@�+@���@��H@���@�M�@�p�@�V@�Ĝ@�I�@�(�@��@�t�@�;d@�
=@���@���@���@�ff@�M�@�E�@�=q@�{@�@��^@��h@��@�`B@�?}@�7L@��/@�j@�1@�ƨ@��@�l�@���@�v�@��@�@��T@���@��#@���@���@���@���@���@���@���@���@���@�@���@���@���@�`B@���@�j@�(�@�  @l�@~�R@~5?@}��@}`B@|�@{��@{"�@zM�@y�@yx�@xĜ@xA�@w�@w|�@wl�@v��@v��@u�@u?}@t�/@t�j@t��@s��@s��@s33@q&�@j��@b�!@Z�!@P�`@I�7@E?}@>�y@9�7@0r�@(�`@$�@�w@o@V@J@�@��@  @�m@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�1A��A��mA��A���A�~�A�bNA�(�A�  A��!A�dZA�$�A��
A�x�A�(�A��A�ĜA��A��wA���A��9A��A�+A�M�A���A�=qA��-A��A���A��A���A�A�A�ĜA��;A�dZA�-A�
=A��A��wA��A��uA�r�A��A���A��A�VA�(�A��TA��7A��DA���A���A�K�A��/A��DA�l�A�S�A�A��hA�ĜA�33A��A��uA��A�JA��hA�33A��A�=qA�oA�wA�A~=qA{�Ay�;Ax�DAw�AwK�Aw+Av��Au|�As�^ArM�Aq��Aqt�Ap�!AnĜAj�\AghsAf�9AfZAf  Ae?}Ad��AbĜA`��A` �A_XA^�\A]A]�A\�uA[�AZ��AY�TAX�AV�AU��AU/AU%AT�yAT�+ASdZAR1'AQ��AP=qAO+AN�HAN(�AK�TAJ��AI�7AH��AH  AF�AC�AB��AB�AAG�A?�A?�A=��A<z�A< �A;A:��A9t�A8�+A7�FA6�RA4��A3��A333A2ȴA2bNA1��A1&�A0  A.jA,�yA,A*v�A)dZA(��A'l�A'G�A'C�A'C�A&��A&�+A&I�A%�FA%dZA%7LA$�9A#�FA"�!A!G�A�
A��A�A��A%A��Ar�A��A�A+AA��A�A��AȴA��A5?A��A��A^5A�#AXA��AE�A�FAO�AjAp�A�A�AVA�A
E�A	�FA	�A	7LA�HA(�AS�A%AȴA�A �A�mA��A�uAjAVA1'A�PA�@�o@��H@�1@�S�@��h@�S�@��@�Q�@��
@�@�{@�7L@�Z@���@��@�@��@��@��@�7@�9@�9X@�K�@ߥ�@�A�@�V@�%@؃@�1'@׾w@�
=@ԣ�@�ff@���@Ϯ@�@�dZ@���@��@�5?@��#@ŉ7@�V@Ĵ9@ēu@�1@�@�b@�\)@��@�V@�J@���@���@�I�@��@��R@�5?@�Ĝ@�b@��F@�K�@���@�5?@�7L@�r�@��@���@�%@��@���@�&�@��9@��@��@�t�@�+@��R@���@�O�@��@���@��@�I�@�1'@�b@��@��;@�ƨ@��P@��@��@���@���@�v�@�M�@�=q@��@���@�hs@�%@��u@�j@�A�@�b@�33@���@��T@�&�@��@�ƨ@��@��@���@�^5@�M�@�=q@�$�@�{@���@���@�hs@��@��@�z�@���@�C�@���@���@��+@��+@�^5@��#@�7L@���@��@��@�^5@�M�@�-@�J@�J@��T@�`B@�Ĝ@�bN@�9X@�1'@� �@��@�b@��@�ƨ@�ƨ@��F@�33@�-@��^@�G�@��@��`@��@�(�@��@�ƨ@�+@�~�@�ff@�=q@���@�G�@��@��`@���@���@��u@�r�@�j@�bN@�Z@�(�@��@�+@���@��H@���@�M�@�p�@�V@�Ĝ@�I�@�(�@��@�t�@�;d@�
=@���@���@���@�ff@�M�@�E�@�=q@�{@�@��^@��h@��@�`B@�?}@�7L@��/@�j@�1@�ƨ@��@�l�@���@�v�@��@�@��T@���@��#@���@���@���@���@���@���@���@���@���@�@���@���@���@�`B@���@�j@�(�@�  @l�@~�R@~5?@}��@}`B@|�@{��@{"�@zM�@y�@yx�@xĜ@xA�@w�@w|�@wl�@v��@v��@u�@u?}@t�/@t�j@t��@s��@s��@s33@q&�@j��@b�!@Z�!@P�`@I�7@E?}@>�y@9�7@0r�@(�`@$�@�w@o@V@J@�@��@  @�m@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBQ�BO�BM�BQ�BT�BVB]/BcTBcTBcTBbNB_;BZBS�BL�BJ�BE�B;dB49B0!B�BVB��B�B�
BB�3B��B��B�+Bo�BZB,B%B�B�BB�B��B��BɺBBB��B�qB�RB�FB�9B�-B�B��B�\B�Bw�Bn�BcTBZBVBR�BC�B(�B�B
=BB
��B
�B
�TB
ǮB
�B
��B
��B
�oB
�JB
�B
z�B
cTB
W
B
M�B
F�B
D�B
B�B
>wB
6FB
(�B
�B
�B
�B
VB	��B	�
B	�}B	�XB	�FB	�3B	�B	��B	��B	�\B	�JB	�1B	�B	~�B	z�B	u�B	p�B	jB	dZB	W
B	K�B	E�B	B�B	A�B	?}B	<jB	6FB	49B	2-B	+B	#�B	!�B	�B	VB	+B	B��B��B�B�sB�fB�`B�`B�B�B�sB�B�B�B�B�fB�NB�5B�B��B��B��B��B��B��BȴBÖB�qB�RB�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�JB�+B�B~�Bz�Bv�Bu�Br�Bn�Bk�BiyBiyBiyBhsBhsBgmBffBe`BcTBaHB_;B^5B\)BZBXBW
BT�BP�BL�BJ�BH�BE�BC�BB�BA�B@�B?}B=qB<jB;dB:^B:^B9XB8RB7LB6FB5?B5?B49B2-B.B)�B%�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B{B�B�B�B�B�B�B�B�B�B�B �B �B�B�B�B'�B)�B)�B,B,B-B.B.B/B1'B1'B49B6FB7LB7LB8RB9XB:^B;dB;dB>wB@�BD�BG�BI�BJ�BL�BM�BN�BO�BP�BR�BVBW
BXBYBYBZBZBZB[#B[#B[#B]/B]/B^5B^5B^5B^5B^5B_;B`BB`BBaHBcTBcTBdZBdZBffBgmBjBm�Bm�Bs�Bv�Bw�Bx�By�Bz�Bz�Bz�B{�B{�B}�B~�B� B�B�B�%B�7B�JB�PB�PB�PB�PB�\B�oB�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�!B�B�B�'B�LB�XB�jB�qB�wB��BĜBĜBǮB��B��B��B��B�B�B�#B�)B�)B�/B�5B�;B�BB�BB�BB�NB�fB�B�B�B�B�B��B��B��B	B	B	%B	
=B	JB	VB	oB	uB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	"�B	%�B	,B	0!B	33B	6FB	7LB	<jB	A�B	E�B	F�B	G�B	H�B	H�B	H�B	H�B	I�B	I�B	I�B	H�B	I�B	I�B	I�B	I�B	I�B	I�B	H�B	L�B	R�B	W
B	[#B	]/B	aHB	e`B	gmB	iyB	l�B	p�B	v�B	x�B	}�B	� B	�B	�B	�+B	�=B	�DB	�DB	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	�B	ǮB	�5B	�B
B
uB
�B
%�B
.B
:^B
E�B
J�B
P�B
W
B
]/B
cTB
ffB
jB
p�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BQ�BO�BM�BQ�BT�BU�B]Bc4Bc5Bc1Bb-B_BY�BS�BL�BJ�BE}B;=B4B/�B�B/B��B�B��B�iB�B��B�zB�BotBY�B+�B�B�wB�B��B��BкBɐB�iB�jB�`B�JB�(B�B�B�B��B��B�3B��Bw�BnpBc-BY�BU�BR�BCpB(�BeB
B�B
��B
�B
�,B
ǆB
��B
��B
�`B
�IB
�$B
��B
z�B
c.B
V�B
M�B
F�B
DvB
BlB
>SB
6%B
(�B
�B
uB
bB
2B	��B	��B	�\B	�5B	�$B	�B	��B	��B	�B	�<B	�+B	�B	��B	~�B	z�B	u�B	p�B	jaB	d;B	V�B	K�B	E�B	BqB	AmB	?aB	<LB	6'B	4B	2B	*�B	#�B	!�B	�B	;B	B	 �B��B��B��B�WB�IB�EB�FB�pB�iB�WB�B�B�B�mB�HB�2B�B�B��B��B��B��B͵B˪BȚB�zB�UB�7B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�nB�UB�0B�B��B~�Bz�Bv�Bu�Br�Bn�BkkBi_Bi`Bi`BhYBhYBgUBfMBeGBc=Ba.B_#B^B\BZBW�BV�BT�BP�BL�BJ�BH�BE�BC}BBxBAqB@jB?dB=>B<RB;LB:CB:CB9$B87B74B6,B5
B5%B4!B2B-�B)�B%�B#�B"�B!�B�B�B�B�B�B�B�B�B�B�B�BmBrBsBrBpBkBgByBmBgBNBmBnBQBiBFBGBTBSBSBnBuBgB�B�B�B�B �B �B�B�B�B'�B)�B)�B+�B+�B,�B-�B-�B.�B1B1
B4B6)B71B7/B85B9;B:@B;GB;FB>YB@eBD�BG�BI�BJ�BL�BM�BN�BO�BP�BR�BU�BV�BW�BX�BX�BY�BY�BY�B[B[B[B]B]B^B^B^B^B^B_B`&B`#Ba)Bc3Bc2Bd8Bd8BfFBgMBjaBmoBmoBs�Bv�Bw�Bx�By�Bz�Bz�Bz�B{�B{�B}�B~�B�B��B��B�B�B�(B�/B�-B�0B�+B�<B�NB�SB�zB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�&B�3B�DB�MB�RB�cB�xB�uBǊBˣB��B��B��B��B��B��B�B�B�	B�B�B�B�B�B�)B�?B�_B�nB�iB�pB�~B��B��B��B	�B	�B	�B	
B	#B	/B	FB	KB	LB	ZB	]B	\B	]B	lB	rB	�B	�B	�B	 �B	"�B	"�B	%�B	+�B	/�B	3B	6B	7!B	<@B	A`B	ExB	F~B	G�B	H�B	H�B	H�B	H�B	I�B	I�B	I�B	H�B	I�B	I�B	I�B	I�B	I�B	I�B	H�B	L�B	R�B	V�B	Z�B	]B	aB	e4B	gBB	iOB	l`B	pxB	v�B	x�B	}�B	�B	��B	��B	��B	�B	�B	�B	�%B	�.B	�IB	�]B	�oB	�oB	�sB	��B	��B	��B	��B	ǃB	�B	�lB
�B
FB
nB
%�B
-�B
:,B
EpB
J�B
P�B
V�B
\�B
c#B
f4B
jMB
ppB
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.41 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708242016053117082420160531170824  AO  ARCAADJP                                                                    20150406191634    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150406191634  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150406191634  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170824  IP                  G�O�G�O�G�O�                