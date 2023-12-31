CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-17T09:15:38Z AOML 3.0 creation; 2016-08-07T21:17:37Z UW 3.1 conversion     
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
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150617091538  20160807141738  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               8A   AO  5285_8895_056                   2C  D   APEX                            6487                            072314                          846 @�Y	�Hp
1   @�Y
\��@-ȴ9Xb�c�A�7K�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    8A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8  B@ffBH  BO��BX  B`ffBg��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3DyffD�fD�Y�D�� D�� D�	�D�C3D�i�D�� D� D�I�D�|�DǶfD��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A�\A&�\AF�\Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B*
=B2
=B9��BB
=BI��BQ=qBY��Bb
=Bi=qBq=qBy��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B���B���B���B���B���B���B�B���B���B���C h�Ch�Ch�Ch�Ch�C
h�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C��Ch�Ch�C h�C"h�C$h�C&h�C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^h�C`h�Cbh�Cdh�Cfh�Chh�Cj��Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�'�C�'�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�AHC�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	=D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D% �D%��D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd�=De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm�=Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt�pDy��D�#�D�f�D��D��D��D�PRD�v�D��D�D�V�D���D�ÅD��R1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�$�A�&�A�(�A�-A�-A�+A�"�A��A��A��A��A��A��A��A��A�JA�VA�%A�A�A���A��A��A���A���A�ffA�oA�M�A�I�A��;A��A�9XAȾwA�bNA���A�9XAƮA�JA�p�A���A�\)A�{A�(�A���A��A���A�ȴA��A�ZA��A�ffA��9A��A�bNA��\A���A���A�`BA�ƨA�A��9A���A���A��7A�ZA�-A��`A��A�$�A�l�A�1A���A��+A��\A�&�A�K�A���A���A�z�A��7A�|�A���A�v�A���A�;dA�7LA��RA�=qA�9XA��A��DA�JA�+A}�wA{hsAuS�Ah-Ad�9A_�PA\~�AXȴAT$�AQ��AQ;dAQ"�AP��APn�AO�^ALA�AG?}AE7LACO�AB�DAA�
AA\)A@M�A>�\A<ȴA;;dA:JA7�7A5K�A4��A4r�A3/A2�jA3��A3�A2�A0�A1��A0��A0M�A/�#A,��A+�PA+t�A+x�A+`BA+"�A*{A)&�A)33A(�`A'�7A&jA%�7A%K�A%|�A%��A%"�A$n�A#�FA#�A#l�A"��A"=qA �/A  �A?}AVAbA��A�AƨAG�A;dA/A�A�A��A�9A�RA�jAVA�A�#A�A�DA5?A��A�FAjA��A��A^5A�A��A��AS�A��A\)A?}AK�AC�A�A{A��A\)A��A�!A~�A1AoA�Ar�A �A��Al�AdZAK�A;dA/A�AȴA��A�uAz�A  Al�A
M�A	��A	&�A�uA�#A�7AXA33A�AQ�A  A|�A��A�\A$�A��A�7A�A��A�\A�PA/A ��A ��A �!A -@��w@��@�t�@�ƨ@��;@��
@�;d@�5?@�/@�1'@��@�l�@�"�@�ff@�@�&�@���@�I�@��m@���@��@�\)@�$�@��h@���@� �@�\)@��@�7L@�bN@�  @�ƨ@�F@��;@�w@�!@��@�@�\@�-@�=q@�V@�5?@�%@�j@���@��`@�9@�I�@�|�@�M�@��@���@�O�@���@��@�"�@◍@��@�l�@��H@�p�@�C�@��@٩�@ف@��@�Ĝ@�Z@��@�l�@���@�v�@�@�`B@�/@�%@�Ĝ@�I�@ӶF@��@�M�@Ѳ-@с@�O�@У�@�1@υ@·+@�Ĝ@�Z@�I�@�A�@�A�@�1'@�  @˾w@�|�@�\)@�"�@��@�E�@��#@ɡ�@�%@���@�r�@�Z@�I�@�(�@��@���@ǅ@���@�^5@�J@�V@�b@î@�t�@��@��y@\@�^5@�5?@�{@��T@��h@�hs@�G�@�&�@���@��@�Z@��@���@�+@��H@�{@�`B@��j@�I�@��
@���@�S�@�
=@�ȴ@�v�@��@���@��h@�?}@��@���@���@�(�@�ƨ@��@���@�l�@�o@���@��/@� �@��@��P@��@��@���@���@��@�p�@��@�1'@���@��H@�ff@�V@�5?@��@�`B@��@���@�Q�@��@��
@��
@��;@��
@�ƨ@���@�C�@�"�@���@�^5@��@�p�@�?}@��j@�bN@��
@�|�@�"�@�ȴ@�^5@�V@�5?@��h@�?}@���@�z�@��@��w@���@���@�J@���@��@��D@�9X@��
@���@��@�\)@�C�@�33@��y@��R@���@�E�@���@�x�@�G�@��`@�j@�9X@�  @�dZ@�;d@���@���@��\@�hs@���@��D@�M�@�P@vV@nv�@d�@]�-@T�@L�j@EO�@<j@6��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  A�$�A�&�A�(�A�-A�-A�+A�"�A��A��A��A��A��A��A��A��A�JA�VA�%A�A�A���A��A��A���A���A�ffA�oA�M�A�I�A��;A��A�9XAȾwA�bNA���A�9XAƮA�JA�p�A���A�\)A�{A�(�A���A��A���A�ȴA��A�ZA��A�ffA��9A��A�bNA��\A���A���A�`BA�ƨA�A��9A���A���A��7A�ZA�-A��`A��A�$�A�l�A�1A���A��+A��\A�&�A�K�A���A���A�z�A��7A�|�A���A�v�A���A�;dA�7LA��RA�=qA�9XA��A��DA�JA�+A}�wA{hsAuS�Ah-Ad�9A_�PA\~�AXȴAT$�AQ��AQ;dAQ"�AP��APn�AO�^ALA�AG?}AE7LACO�AB�DAA�
AA\)A@M�A>�\A<ȴA;;dA:JA7�7A5K�A4��A4r�A3/A2�jA3��A3�A2�A0�A1��A0��A0M�A/�#A,��A+�PA+t�A+x�A+`BA+"�A*{A)&�A)33A(�`A'�7A&jA%�7A%K�A%|�A%��A%"�A$n�A#�FA#�A#l�A"��A"=qA �/A  �A?}AVAbA��A�AƨAG�A;dA/A�A�A��A�9A�RA�jAVA�A�#A�A�DA5?A��A�FAjA��A��A^5A�A��A��AS�A��A\)A?}AK�AC�A�A{A��A\)A��A�!A~�A1AoA�Ar�A �A��Al�AdZAK�A;dA/A�AȴA��A�uAz�A  Al�A
M�A	��A	&�A�uA�#A�7AXA33A�AQ�A  A|�A��A�\A$�A��A�7A�A��A�\A�PA/A ��A ��A �!A -@��w@��@�t�@�ƨ@��;@��
@�;d@�5?@�/@�1'@��@�l�@�"�@�ff@�@�&�@���@�I�@��m@���@��@�\)@�$�@��h@���@� �@�\)@��@�7L@�bN@�  @�ƨ@�F@��;@�w@�!@��@�@�\@�-@�=q@�V@�5?@�%@�j@���@��`@�9@�I�@�|�@�M�@��@���@�O�@���@��@�"�@◍@��@�l�@��H@�p�@�C�@��@٩�@ف@��@�Ĝ@�Z@��@�l�@���@�v�@�@�`B@�/@�%@�Ĝ@�I�@ӶF@��@�M�@Ѳ-@с@�O�@У�@�1@υ@·+@�Ĝ@�Z@�I�@�A�@�A�@�1'@�  @˾w@�|�@�\)@�"�@��@�E�@��#@ɡ�@�%@���@�r�@�Z@�I�@�(�@��@���@ǅ@���@�^5@�J@�V@�b@î@�t�@��@��y@\@�^5@�5?@�{@��T@��h@�hs@�G�@�&�@���@��@�Z@��@���@�+@��H@�{@�`B@��j@�I�@��
@���@�S�@�
=@�ȴ@�v�@��@���@��h@�?}@��@���@���@�(�@�ƨ@��@���@�l�@�o@���@��/@� �@��@��P@��@��@���@���@��@�p�@��@�1'@���@��H@�ff@�V@�5?@��@�`B@��@���@�Q�@��@��
@��
@��;@��
@�ƨ@���@�C�@�"�@���@�^5@��@�p�@�?}@��j@�bN@��
@�|�@�"�@�ȴ@�^5@�V@�5?@��h@�?}@���@�z�@��@��w@���@���@�J@���@��@��D@�9X@��
@���@��@�\)@�C�@�33@��y@��R@���@�E�@���@�x�@�G�@��`@�j@�9X@�  @�dZ@�;d@���@���G�O�@�hs@���@��D@�M�@�P@vV@nv�@d�@]�-@T�@L�j@EO�@<j@6��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�FB	�LB	�LB	�LB	�LB	�LB	�LB	�LB	�LB	�LB	�FB	�9B	��B
)�B
XB
�PB
�`BgmB�FB�B�B+BA�BJ�BXBgmBt�B�B�B�{B�'BÖB�B��B��BhB�B"�B&�B(�B,B+B�B�TB��B�RB��B� Bk�BaHBbNBhsBhsBe`BaHBS�BM�BK�BI�B>wB%�B{B��B�B�}B�?B��B|�BQ�B,B�BB
�B
��B
�RB
��B
�1B
M�B
�B	��B	�HB	ɺB	��B	@�B	#�B	
=B��B�B�TB�5B�)B�)B�#B�B�B�
B�mB�sB�B�B	B	�B	)�B	B�B	dZB	t�B	�7B	�B	�+B	�7B	�JB	��B	�9B	��B	�B	�B	�TB	��B	��B	��B	��B	�BB	�`B	��B
  B
uB
uB
VB
uB
�B
�B
oB
VB
JB
JB
bB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
uB
#�B
(�B
"�B
"�B
)�B
)�B
)�B
8RB
<jB
9XB
?}B
E�B
F�B
H�B
J�B
I�B
F�B
D�B
B�B
E�B
?}B
;dB
9XB
2-B
-B
/B
8RB
5?B
0!B
(�B
,B
2-B
6FB
8RB
6FB
:^B
;dB
;dB
;dB
;dB
;dB
:^B
9XB
8RB
8RB
7LB
9XB
9XB
9XB
9XB
8RB
8RB
8RB
8RB
7LB
6FB
5?B
49B
2-B
2-B
0!B
0!B
/B
.B
.B
-B
+B
)�B
(�B
&�B
%�B
$�B
#�B
"�B
!�B
"�B
 �B
�B
�B
�B
 �B
 �B
!�B
 �B
!�B
!�B
"�B
%�B
'�B
)�B
(�B
'�B
&�B
'�B
'�B
'�B
'�B
&�B
%�B
$�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
$�B
#�B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
\B
VB
PB
PB
\B
\B
PB
VB
bB
bB
hB
hB
oB
hB
bB
bB
bB
\B
\B
PB
DB
+B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
1B
+B
%B
+B
+B
+B
+B
+B
+B
+B
1B
1B
1B

=B

=B
DB
DB
JB
\B
bB
bB
bB
bB
bB
\B
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
VB
VB
\B
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
#�B
(�B
-B
33B
9XB
?}B
E�B
G�B
M�B
P�B
W
B
]/B
^51111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  B	�2B	�1B	�1B	�1B	�1B	�1B	�0B	�0B	�2B	�2B	�0B	�2B	�2B	�0B	�7B	�7B	�5B	�5B	�7B	�7B	�8B	�8B	�6B	�/B	�%B	��B
)�B
W�B
�4B
�ABgNB�#B�B|B*�BAfBJ�BW�BgJBt�B��B��B�YB�B�qB��B��B��BEB�B"�B&�B(�B+�B*�B�B�0B��B�0B�iB�BkbBa#Bb+BhPBhOBe8Ba&BS�BM�BK�BI�B>RB%�BWB��B��B�WB�B��B|�BQ�B+�BzB �B
�bB
��B
�/B
��B
�B
M�B
�B	��B	�%B	ɚB	�jB	@hB	#�B	
!B��B�~B�;B�B�B�B�B�B��B��B�QB�UB�eB�|B	B	�B	)�B	BnB	d9B	t�B	�B	��B	�B	�B	�&B	��B	�B	��B	�kB	�eB	�,B	��B	��B	��B	��B	�B	�9B	��B	��B
LB
NB
-B
OB
iB
jB
FB
.B
#B
#B
:B
}B
�B
jB
VB
�B
�B
�B
�B
�B
uB
YB
>B
KB
#�B
(�B
"�B
"�B
)�B
)�B
)�B
8(B
<?B
9,B
?QB
EwB
F{B
H�B
J�B
I�B
F|B
DrB
BeB
EuB
?RB
;8B
9,B
2B
,�B
.�B
8(B
5B
/�B
(�B
+�B
2B
6B
8'B
6B
:0B
;7B
;9B
;9B
;:B
;;B
:3B
9-B
8'B
8&B
7"B
9,B
9-B
9-B
9+B
8'B
8$B
8(B
8%B
7B
6B
5B
4B
2B
2B
/�B
/�B
.�B
-�B
-�B
,�B
*�B
)�B
(�B
&�B
%�B
$�B
#�B
"�B
!�B
"�B
 �B
�B
�B
�B
 �B
 �B
!�B
 �B
!�B
!�B
"�B
%�B
'�B
)�B
(�B
'�B
&�B
'�B
'�B
'�B
'�B
&�B
%�B
$�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
$�B
#�B
"�B
�B
{B
nB
bB
\B
bB
bB
lB
gB
XB
;B
0B
)B
#B
!B
0B
.B
#B
*B
5B
6B
;B
:B
?B
;B
5B
6B
5B
1B
/B
"B
B
B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 B
�B
�B
�B
 B
�B
�B
B
�B
B
B
B

B

B
B
B
B
-B
4B
4B
5B
4B
5B
0B
'B
-B
.B
.B
.B
.B
+B
,B
/B
-B
.B
-B
,B
-B
/B
,B
.B
-B
/B
+B
,B
%B
'B
.B
GB
LB
LB
MB
LB
KB
SB
OB
RB
WB
YB
]B
XB
ZB
XB
RB
LB
QB
PB
WG�O�B
�B
#�B
(�B
,�B
3B
9(B
?NB
EqB
G~B
M�B
P�B
V�B
\�B
^1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.41 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417382016080714173820160807141738  AO  ARCAADJP                                                                    20150617091538    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150617091538  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150617091538  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141738  IP                  G�O�G�O�G�O�                