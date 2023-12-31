CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-13T18:36:48Z creation;2018-11-13T18:36:51Z conversion to V3.1;2019-12-23T06:12:03Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181113183648  20200120021521  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               gA   JA  I2_0675_103                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؐa��� 1   @ؐb��-�@6�~���$�cN��f�B1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|fD|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@��A�\A&�\AF�\Af�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Br
=By=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C h�Ch�Ch�Ch�Ch�C
h�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�Ch�C h�C"h�C$h�C&h�C(h�C*h�C,h�C.h�C0h�C2h�C4h�C6h�C8h�C:h�C<h�C>h�C@h�CBh�CDh�CFh�CHh�CJh�CLh�CNh�CPh�CRh�CTh�CVh�CXh�CZh�C\h�C^h�C`h�Cbh�Cdh�Cfh�Chh�Cjh�Clh�Cnh�Cph�Crh�Cth�Cvh�Cxh�Czh�C|h�C~h�C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{C�4{D =D �=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D	 �D	�=D
=D
�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D=D�=D =D �=D!=D!�=D"=D"�=D#=D#�=D$=D$�=D%=D%�=D&=D&�=D'=D'�=D(=D(�=D)=D)�=D*=D*�=D+=D+�=D,=D,�=D-=D-�=D.=D.�=D/=D/�=D0=D0�=D1=D1�=D2=D2�=D3=D3�=D4=D4�=D5=D5�=D6=D6�=D7=D7�=D8=D8�=D9=D9�=D:=D:�=D;=D;�=D<=D<�=D==D=�=D>=D>�=D?=D?�=D@=D@�=DA=DA�=DB=DB�=DC=DC�=DD=DD�=DE=DE�=DF=DF�=DG=DG�=DH=DH�=DI=DI�=DJ=DJ�=DK=DK�=DL=DL�=DM=DM�=DN=DN�=DO=DO�=DP=DP�=DQ=DQ�=DR=DR�=DS=DS�=DT=DT�=DU=DU�=DV=DV�=DW=DW�=DX=DX�=DY=DY�=DZ=DZ�=D[=D[�=D\=D\�=D]=D]�=D^=D^�=D_=D_�=D`=D`�=Da=Da�=Db=Db�=Dc=Dc�=Dd=Dd��De=De�=Df=Df�=Dg=Dg�=Dh=Dh�=Di=Di�=Dj=Dj�=Dk=Dk�=Dl=Dl�=Dm=Dm��Dn=Dn�=Do=Do�=Dp=Dp�=Dq=Dq�=Dr=Dr�=Ds=Ds�=Dt=Dt�=Du=Du�=Dv=Dv�=Dw=Dw�=Dx=Dx�=Dy=Dy�=Dz=Dz�=D{=D{�=D| �D|�=D}=D}�=D~=D~�=D=D�=D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD���D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�RD�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��RD�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MDD��D�D�MDÍD��D�D�MDčD��D�D�MDōD��D�D�MDƍD��D�D�MDǍD��D�D�MDȍD��D�D�MDɍD��D�D�MDʍD��D�D�MDˍD��D�D�MD̍D��D�D�MD͍D��D�D�MD΍D��D�D�MDύD��D�D�MDЍD��D�D�MDэD��D�D�MDҍD��D�D�MDӍD��D�D�MDԍD��D�D�MDՍD��D�D�MD֍D��D�D�MD׍D��D�D�MD؍D��D�D�MDٍD��D�D�MDڍD��D�D�MDۍD��D�D�MD܍D��D�D�MDݍD��D�D�MDލD��D�D�MDߍD��D�D�MD��D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�PRD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�PRD�D��D�D�MD�D��D�D�MD�D��D�D�MD��D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD�D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�MD��D��D�D�PRD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aϰ!Aϲ-AϬAϰ!Aϰ!Aϟ�AυA�/A�"�A��A�VA�A���A���A��A��`A��;A��/A��A���A�ĜAκ^AζFAΧ�A�z�A�^5A�S�A�33A���A̶FA�r�A���A��!A��\A���A�K�A�9XA�ȴA��A��A���A��A��A���A�\)A��TA���A�p�A���A�;dA�\)A���A���A��+A�33A��mA��A�S�A��A�1A��wA�1'A�^5A���A���A�ZA��wA��A��A��A���A�1A�jA���A�t�A���A��jA��jA�(�A�+A���A��9A���A�1'A���A��jA�9XA��HA�=qA�hsA�A�A�&�A��DA�ZA��+A���A���A��\A���A���A��A�l�A�^5A}�mA|��A{�PA{x�Az�RAx��Axv�AxZAw;dAr��ApQ�An�\AmS�Aj��Ag�^Ad5?AbAa�Aa��A`��A_;dA]p�A\�AZ��AZA�AWXAUp�AT�!AT�AQG�AM"�AJE�AIƨAIhsAH�RAF�yAE�-AC�
AAdZA?
=A=�7A="�A<I�A:ffA9�A7�#A7VA6�DA5�A4~�A3�A25?A1��A/�A-�A,ȴA,r�A+�FA+;dA*�HA)l�A'A&ffA$1A"�!A ��A�`AJA1Ax�A"�AjA�hA�A-A+A+A�A�hA5?A&�A��A�AVA�AĜA��AZAx�A��A�FAK�A�AdZA	K�A�HA�PAn�Ax�Ar�A�AdZA��Ap�A �A �@���@�G�@���@���@��\@��@�G�@���@�1@��T@���@�@��#@��`@�V@�@���@�!@�G�@�Z@��m@���@���@ߍP@�n�@ݩ�@�X@���@��m@��H@���@؋D@�Q�@�t�@ָR@�~�@���@�b@Ӆ@�dZ@�
=@ҧ�@Ѳ-@Гu@�9X@Ϯ@�o@�$�@˅@�E�@��@��@���@ɡ�@�7L@���@�S�@���@�=q@�p�@��/@�1'@Õ�@�33@��@¸R@�n�@���@�X@��P@�+@�V@���@�O�@�j@�ƨ@��H@�X@���@�I�@��@�l�@��H@�ff@�O�@�b@��@��@�=q@��#@��^@��@���@�9X@�33@�M�@��@�A�@��F@�C�@��!@���@���@��+@�-@��@��@�I�@�1@��w@��@��@��R@�5?@�O�@��@���@���@�Z@�(�@�1@��@���@���@�K�@�+@���@���@���@��@��@��@��@�V@�Ĝ@�I�@��w@���@�|�@�C�@�"�@���@�V@��T@��@�%@���@���@���@�bN@�b@��@��@�;d@��y@�$�@��@�@�%@��j@�1@�C�@��!@�E�@��@���@��7@�/@�V@���@�%@�V@�bN@�1@�\)@��@�ȴ@�ȴ@��R@���@�v�@��@��7@�7L@�Ĝ@��@��D@��@�z�@���@���@�%@��@���@��/@���@�z�@���@��;@��@���@�$�@�J@��7@�%@���@��@�V@�?}@�X@�%@��u@�Q�@�1@��
@��P@�S�@�33@�"�@�
=@��y@���@��R@��+@�5?@�5?@�-@�{@�{@�J@���@���@���@���@��@��@�r�@�I�@�  @�  @��@��
@��w@�ƨ@���@�ƨ@�ƨ@�ƨ@��w@��w@���@�|�@�dZ@�C�@�33@�"�@�o@��@�n�@�@���@���@�hs@�G�@�/@��/@��u@�j@�I�@��@��@�ƨ@���@�dZ@�"�@�"�@�o@��@��R@���@�~�@�^5@�M�@�$�@��#@���@��7@�?}@��/@��@�bN@�b@�;@��@+@~ȴ@~5?@}�-@}p�@}?}@|�@|I�@|1@{S�@z�@z��@z�\@z-@y�#@y&�@x�9@x1'@w��@w|�@vȴ@v@u�@t��@t�D@t9X@s��@s��@s�@so@r��@r=q@r�@q�7@q7L@q&�@pĜ@o�@n�y@n�R@n��@n��@n��@n��@n�+@n{@m�@l�j@l��@l�D@lz�@l(�@kƨ@k�@kt�@k"�@j�H@jn�@j�@i�^@i7L@i%@h��@h�9@h �@g�P@g+@f��@e@e�h@ep�@e`B@e?}@d�@d�D@c�
@ct�@c"�@b��@b��@b��@b��@b��@b~�@b=q@a�@a��@ax�@ahs@aG�@`��@`�9@`bN@`b@_�P@^��@^��@^5?@^{@]@]��@]��@]�h@]�@\�/@\z�@\9X@[�
@[�@[C�@[o@Z�!@ZM�@Y��@Y�^@Yx�@YX@YG�@Y7L@Xr�@W�@Wl�@V�R@V��@VV@U@U?}@UV@T��@TZ@TI�@S��@S�
@S�F@SdZ@S@R~�@R-@Q�@Q�^@Q��@Q7L@P�`@P��@P �@O��@O\)@O+@Nȴ@N�+@Nff@NE�@N$�@M�@MO�@L�/@L�j@LZ@LI�@L9X@L9X@L(�@K�F@Kt�@KS�@K33@J�@J�!@J~�@J-@I�^@I&�@Hr�@HQ�@HA�@H �@G�;@G�P@G;d@F��@F��@F�y@F��@E�T@E��@E�h@E�@E`B@E/@D�@D�j@D(�@C�
@C�F@CC�@Co@B��@B-@A��@A��@Ax�@AG�@@��@@A�@@  @?�@?;d@>�@>��@>ff@>{@>@=�T@=�@=�@<�@<I�@<1@;��@;t�@;33@;@:�H@:��@:~�@:^5@:^5@:=q@9��@9G�@97L@9&�@8�9@8�@81'@8  @7��@7��@7+@6�@6��@6E�@5�@5`B@5�@4�@4�j@4�@4z�@4(�@3t�@333@2��@2^5@2=q@2=q@2-@2J@1�@1�^@1��@1hs@0�`@0��@0bN@0  @/�;@/��@/�w@/l�@/�@.�R@.V@.$�@-�@-�T@-�h@-p�@-`B@-V@,�j@,�@,�D@,(�@+��@+��@+t�@+33@*��@*�@)�#@)��@)�^@)7L@(Ĝ@(r�@(1'@(  @'��@'�P@&��@&�+@&E�@&5?@&$�@%@%p�@%?}@$��@$�D@$I�@$1@#�m@#��@#o@"��@"M�@!�#@ ��@ �@  �@�@��@�@l�@+@��@��@��@�@�R@�R@��@�+@V@V@V@V@�@@�-@��@�h@`B@?}@/@�@�j@�D@z�@Z@9X@9X@�@��@ƨ@C�@�H@��@�!@��@�\@n�@�@�#@x�@7L@��@�9@�@A�@  @�;@�@l�@\)@+@
=@��@�y@�y@ȴ@ȴ@��@v�@ff@5?@�T@@�-@�h@p�@`B@?}@V@��@��@�@�j@I�@(�@(�@1@�
@��@dZ@S�@dZ@dZ@S�@o@@@@�H@��@n�@^5@^5@=q@J@��@7L@�`@�@ �@b@  @�@�;@��@�@ȴ@v�@5?@{@�@��@�h@�@p�@p�@��@�@�@I�@9X@�@�m@�@dZ@C�@C�@33@
�@
�\@
=q@
�@
J@	��@	��@	��@	�#@	�^@	��@	X@	�@Ĝ@��@��@Q�@  @�@�;@��@|�@|�@\)@;d@��@��@��@�+@V@5?@@�@��@�@?}@�@�@��@��@�j@z�@Z@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aϰ!Aϲ-AϬAϰ!Aϰ!Aϟ�AυA�/A�"�A��A�VA�A���A���A��A��`A��;A��/A��A���A�ĜAκ^AζFAΧ�A�z�A�^5A�S�A�33A���A̶FA�r�A���A��!A��\A���A�K�A�9XA�ȴA��A��A���A��A��A���A�\)A��TA���A�p�A���A�;dA�\)A���A���A��+A�33A��mA��A�S�A��A�1A��wA�1'A�^5A���A���A�ZA��wA��A��A��A���A�1A�jA���A�t�A���A��jA��jA�(�A�+A���A��9A���A�1'A���A��jA�9XA��HA�=qA�hsA�A�A�&�A��DA�ZA��+A���A���A��\A���A���A��A�l�A�^5A}�mA|��A{�PA{x�Az�RAx��Axv�AxZAw;dAr��ApQ�An�\AmS�Aj��Ag�^Ad5?AbAa�Aa��A`��A_;dA]p�A\�AZ��AZA�AWXAUp�AT�!AT�AQG�AM"�AJE�AIƨAIhsAH�RAF�yAE�-AC�
AAdZA?
=A=�7A="�A<I�A:ffA9�A7�#A7VA6�DA5�A4~�A3�A25?A1��A/�A-�A,ȴA,r�A+�FA+;dA*�HA)l�A'A&ffA$1A"�!A ��A�`AJA1Ax�A"�AjA�hA�A-A+A+A�A�hA5?A&�A��A�AVA�AĜA��AZAx�A��A�FAK�A�AdZA	K�A�HA�PAn�Ax�Ar�A�AdZA��Ap�A �A �@���@�G�@���@���@��\@��@�G�@���@�1@��T@���@�@��#@��`@�V@�@���@�!@�G�@�Z@��m@���@���@ߍP@�n�@ݩ�@�X@���@��m@��H@���@؋D@�Q�@�t�@ָR@�~�@���@�b@Ӆ@�dZ@�
=@ҧ�@Ѳ-@Гu@�9X@Ϯ@�o@�$�@˅@�E�@��@��@���@ɡ�@�7L@���@�S�@���@�=q@�p�@��/@�1'@Õ�@�33@��@¸R@�n�@���@�X@��P@�+@�V@���@�O�@�j@�ƨ@��H@�X@���@�I�@��@�l�@��H@�ff@�O�@�b@��@��@�=q@��#@��^@��@���@�9X@�33@�M�@��@�A�@��F@�C�@��!@���@���@��+@�-@��@��@�I�@�1@��w@��@��@��R@�5?@�O�@��@���@���@�Z@�(�@�1@��@���@���@�K�@�+@���@���@���@��@��@��@��@�V@�Ĝ@�I�@��w@���@�|�@�C�@�"�@���@�V@��T@��@�%@���@���@���@�bN@�b@��@��@�;d@��y@�$�@��@�@�%@��j@�1@�C�@��!@�E�@��@���@��7@�/@�V@���@�%@�V@�bN@�1@�\)@��@�ȴ@�ȴ@��R@���@�v�@��@��7@�7L@�Ĝ@��@��D@��@�z�@���@���@�%@��@���@��/@���@�z�@���@��;@��@���@�$�@�J@��7@�%@���@��@�V@�?}@�X@�%@��u@�Q�@�1@��
@��P@�S�@�33@�"�@�
=@��y@���@��R@��+@�5?@�5?@�-@�{@�{@�J@���@���@���@���@��@��@�r�@�I�@�  @�  @��@��
@��w@�ƨ@���@�ƨ@�ƨ@�ƨ@��w@��w@���@�|�@�dZ@�C�@�33@�"�@�o@��@�n�@�@���@���@�hs@�G�@�/@��/@��u@�j@�I�@��@��@�ƨ@���@�dZ@�"�@�"�@�o@��@��R@���@�~�@�^5@�M�@�$�@��#@���@��7@�?}@��/@��@�bN@�b@�;@��@+@~ȴ@~5?@}�-@}p�@}?}@|�@|I�@|1@{S�@z�@z��@z�\@z-@y�#@y&�@x�9@x1'@w��@w|�@vȴ@v@u�@t��@t�D@t9X@s��@s��@s�@so@r��@r=q@r�@q�7@q7L@q&�@pĜ@o�@n�y@n�R@n��@n��@n��@n��@n�+@n{@m�@l�j@l��@l�D@lz�@l(�@kƨ@k�@kt�@k"�@j�H@jn�@j�@i�^@i7L@i%@h��@h�9@h �@g�P@g+@f��@e@e�h@ep�@e`B@e?}@d�@d�D@c�
@ct�@c"�@b��@b��@b��@b��@b��@b~�@b=q@a�@a��@ax�@ahs@aG�@`��@`�9@`bN@`b@_�P@^��@^��@^5?@^{@]@]��@]��@]�h@]�@\�/@\z�@\9X@[�
@[�@[C�@[o@Z�!@ZM�@Y��@Y�^@Yx�@YX@YG�@Y7L@Xr�@W�@Wl�@V�R@V��@VV@U@U?}@UV@T��@TZ@TI�@S��@S�
@S�F@SdZ@S@R~�@R-@Q�@Q�^@Q��@Q7L@P�`@P��@P �@O��@O\)@O+@Nȴ@N�+@Nff@NE�@N$�@M�@MO�@L�/@L�j@LZ@LI�@L9X@L9X@L(�@K�F@Kt�@KS�@K33@J�@J�!@J~�@J-@I�^@I&�@Hr�@HQ�@HA�@H �@G�;@G�P@G;d@F��@F��@F�y@F��@E�T@E��@E�h@E�@E`B@E/@D�@D�j@D(�@C�
@C�F@CC�@Co@B��@B-@A��@A��@Ax�@AG�@@��@@A�@@  @?�@?;d@>�@>��@>ff@>{@>@=�T@=�@=�@<�@<I�@<1@;��@;t�@;33@;@:�H@:��@:~�@:^5@:^5@:=q@9��@9G�@97L@9&�@8�9@8�@81'@8  @7��@7��@7+@6�@6��@6E�@5�@5`B@5�@4�@4�j@4�@4z�@4(�@3t�@333@2��@2^5@2=q@2=q@2-@2J@1�@1�^@1��@1hs@0�`@0��@0bN@0  @/�;@/��@/�w@/l�@/�@.�R@.V@.$�@-�@-�T@-�h@-p�@-`B@-V@,�j@,�@,�D@,(�@+��@+��@+t�@+33@*��@*�@)�#@)��@)�^@)7L@(Ĝ@(r�@(1'@(  @'��@'�P@&��@&�+@&E�@&5?@&$�@%@%p�@%?}@$��@$�D@$I�@$1@#�m@#��@#o@"��@"M�@!�#@ ��@ �@  �@�@��@�@l�@+@��@��@��@�@�R@�R@��@�+@V@V@V@V@�@@�-@��@�h@`B@?}@/@�@�j@�D@z�@Z@9X@9X@�@��@ƨ@C�@�H@��@�!@��@�\@n�@�@�#@x�@7L@��@�9@�@A�@  @�;@�@l�@\)@+@
=@��@�y@�y@ȴ@ȴ@��@v�@ff@5?@�T@@�-@�h@p�@`B@?}@V@��@��@�@�j@I�@(�@(�@1@�
@��@dZ@S�@dZ@dZ@S�@o@@@@�H@��@n�@^5@^5@=q@J@��@7L@�`@�@ �@b@  @�@�;@��@�@ȴ@v�@5?@{@�@��@�h@�@p�@p�@��@�@�@I�@9X@�@�m@�@dZ@C�@C�@33@
�@
�\@
=q@
�@
J@	��@	��@	��@	�#@	�^@	��@	X@	�@Ĝ@��@��@Q�@  @�@�;@��@|�@|�@\)@;d@��@��@��@�+@V@5?@@�@��@�@?}@�@�@��@��@�j@z�@Z@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBBBÖBÖB��B�B�B�B �B!�B#�B%�B.B7LB?}BB�BG�BJ�BL�BM�BM�BN�BP�BQ�BR�BS�BS�BS�BH�Be`B��B�-B�-B�'B�!B�B��B��B��B��B��B��B��B�uB�bB�VB�\B�DB�B~�B�B}�By�Bz�B�B�B�B�B}�Bx�Bu�Bm�BgmBbNB\)BXBJ�BF�B@�B<jB49B+B!�BhBB��B�ZB�B�'B�uB�\B�JB�Bo�BZB;dB�BJB
��B
�HB
��B
ǮB
ȴB
�mB
��B
�'B
��B
�bB
{�B
q�B
� B
u�B
`BB
T�B
I�B
J�B
H�B
6FB
/B
,B
"�B
B	�B	�BB	�B	ĜB	�!B	��B	�B	�B	�B	{�B	o�B	ffB	^5B	VB	Q�B	E�B	8RB	2-B	-B	!�B	PB��B��B�B�B�sB�TB��B�}B�FB�?B�9B�B��B��B�oB�{B�oB�bB�PB�+B�B{�Br�BjBjBn�Bm�Bk�BjBgmBbNB`BB`BB\)B[#BW
BT�BT�BW
BXBVBT�BQ�BP�BN�BN�BH�BG�BE�BB�B@�BA�B>wB>wB=qB<jB;dB:^B9XB8RB6FB49B5?B1'B0!B0!B.B.B+B+B)�B'�B(�B'�B&�B"�B �B �B �B�B�B�B�B�B �B �B"�B"�B#�B$�B&�B&�B'�B(�B(�B(�B'�B,B+B,B,B,B,B-B.B0!B0!B0!B1'B1'B1'B49B5?B5?B5?B6FB6FB7LB6FB6FB7LB7LB8RB;dB<jB<jB<jB<jB<jB<jB>wB?}B@�BA�BB�BC�BD�BE�BG�BG�BH�BH�BJ�BN�BT�BT�BW
BXBYBZB\)B_;BcTBe`BffBgmBiyBjBjBl�Bo�Bq�Bt�Bv�Bw�Bw�Bz�B|�B~�B�B�1B�hB�oB��B��B��B��B��B��B��B��B��B��B�B�B�-B�?B�FB�^B��BÖBŢBŢBǮBɺB��B��B��B��B��B�
B�B�/B�TB�B�B�B��B	B	+B	
=B	VB	bB	{B	�B	�B	�B	!�B	'�B	+B	/B	0!B	1'B	2-B	49B	5?B	8RB	9XB	;dB	>wB	D�B	G�B	L�B	Q�B	Q�B	W
B	VB	VB	W
B	ZB	^5B	`BB	cTB	e`B	gmB	gmB	jB	jB	jB	o�B	q�B	s�B	u�B	v�B	v�B	w�B	y�B	y�B	{�B	{�B	~�B	�B	�B	�B	�B	�7B	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�?B	�RB	�XB	�^B	�jB	�jB	�jB	�jB	�jB	�wB	�wB	��B	ĜB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�#B	�)B	�/B	�5B	�5B	�5B	�BB	�HB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
+B
1B
1B
1B
	7B

=B

=B

=B

=B
DB
DB
JB
JB
JB
JB
PB
PB
VB
VB
\B
\B
\B
bB
bB
hB
hB
oB
oB
oB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
-B
.B
.B
/B
/B
/B
/B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
2-B
33B
2-B
2-B
2-B
33B
33B
33B
33B
49B
33B
49B
49B
5?B
5?B
49B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
A�B
A�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�[B�uB�uB�{B�{B��B�BB�B �B!�B#�B%�B-�B72B?HBBuBG�BJ�BL�BM�BM�BN�BP�BQ�BR�BS�BS�BS�BH�BeFB��B�B��B�B��B��B��B��B��B��B��B��B�SB�[B�HB�"B�BB�)B�B~�B��B}�By�Bz�B��B��B��B��B}�Bx�Bu�Bm]Bg8BbB\BW�BJ�BF�B@iB<PB4B*�B!�BNBB��B�&B��B��B�@B�(B�B��Bo�BZB;0B�B0B
��B
�-B
ΥB
�zB
ȀB
�8B
͹B
��B
��B
�HB
{�B
qvB
�B
u�B
`B
T�B
I�B
J�B
H�B
6B
.�B
+�B
"�B
�B	�cB	�B	��B	�gB	��B	�eB	��B	��B	��B	{�B	o�B	f2B	^B	U�B	Q�B	E�B	88B	1�B	,�B	!�B	B��B��B�B�vB�XB� B��B�HB�B�%B�B��B��B�eB�:B�FB�:B�.B�B�B��B{�Br|BjKBjeBncBm]BkkBjKBg8BbB`B`B[�BZ�BV�BT�BT�BV�BW�BU�BT�BQ�BP�BN�BN�BH�BGzBEmBB[B@iBAUB>]B>BB=<B<6B;0B:*B9$B8B6B4B5B0�B/�B/�B-�B-�B*�B*�B)�B'�B(�B'�B&�B"�B �B �B �B�B�B�B�B�B �B �B"�B"�B#�B$�B&�B&�B'�B(�B(�B(�B'�B+�B*�B+�B+�B+�B+�B,�B-�B/�B/�B/�B0�B0�B0�B4B5B5B5B5�B6+B7B6B6B6�B7B8B;0B<6B<6B<PB<B<6B<6B>]B?HB@OBAUBB[BCaBD�BEmBGzBGzBH�BH�BJ�BN�BT�BT�BV�BW�BX�BY�B[�B_Bc BeBf2BgRBiDBjKBj0BlWBoiBqvBt�Bv�Bw�Bw�Bz�B|�B~�B��B��B�4B�:B�MB�_B�qB�qB�qB�xB�~B��B��B��B��B��B��B�B�B�*B�UB�aB�mB�mB�_BɆBʌB˒B̘BϫBңB��B��B��B� B�QB�}B�|B��B	�B	�B	
	B	"B	.B	FB	YB	eB	]B	!�B	'�B	*�B	.�B	/�B	0�B	1�B	4B	5B	8B	9$B	;0B	>]B	DgB	GzB	L�B	Q�B	Q�B	V�B	U�B	U�B	V�B	Y�B	]�B	`B	c B	eB	g8B	g8B	jKB	jKB	jKB	oiB	qvB	shB	u�B	v�B	v�B	w�B	y�B	y�B	{�B	{�B	~�B	��B	��B	��B	��B	�B	�B	�@B	�MB	�MB	�MB	�2B	�_B	�_B	�MB	�MB	�FB	�SB	�YB	�eB	�QB	�qB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�$B	�B	�6B	�6B	�6B	�6B	�6B	�BB	�BB	�UB	�gB	�gB	�tB	�lB	˒B	͟B	͟B	ΥB	ΥB	ΥB	ϫB	ѷB	өB	өB	��B	��B	��B	յB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	� B	� B	�&B	�,B	�2B	�8B	�8B	�8B	�>B	�>B	�DB	�DB	�DB	�DB	�0B	�QB	�WB	�WB	�]B	�iB	�iB	�oB	�vB	�vB	�vB	�|B	�B	�B	�B	�B	�B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
	�B

	B

	B

	B

�B
B
B
B
B
B
B
B
B
"B
(B
B
(B
.B
.B
4B
B
:B
:B
TB
:B
:B
 B
&B
FB
FB
FB
MB
SB
SB
SB
SB
SB
?B
YB
YB
YB
YB
YB
EB
_B
KB
kB
kB
qB
xB
xB
xB
~B
~B
~B
~B
~B
~B
�B
�B
�B
�B
jB
�B
pB
�B
 �B
 �B
 �B
 �B
 �B
 �B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
-�B
-�B
.�B
.�B
.�B
.�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
1�B
2�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
4B
2�B
4B
4B
5%B
5B
3�B
4�B
5%B
5B
5B
5�B
6B
6B
6B
6B
7B
7B
7B
8B
8B
8B
9	B
9$B
:*B
:*B
:*B
;0B
;0B
;0B
;0B
;0B
;B
<B
<6B
="B
>(B
>BB
>(B
>(B
>BB
?HB
?HB
?HB
?HB
?HB
?HB
?HB
@OB
A;B
AUB
@OB
AUB
AUB
B[B
B[B
BAB
BAB
CaB
CaB
CaB
DgB
DgB
ESB
EmB
EmB
EmB
EmB
EmB
EmB
FtB
FtB
GzB
GzB
GzB
G_B
GzB
GzB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
I�B
I�B
J�B
JrB
KxB
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
ZB
[	B
[	B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^B
^B
^B
]�B
^B
^B
_B
_B
^�B
_B
_B
_�B
_�B
`B
aB
aB
aB
aB
a-B
a-B
aB
aB
bB
a�B
bB
bB
bB
bB
a�B
a�B
bB
c B
c B
c B
c B
c B
c B
c B
c B
c B
c B
c B
c B
d&B
d&B
d@B
dB
dB
e,B
e,B
e,B
e,B
e,B
e,B
e,B
eB
eFB
e,B
f2B
f2B
f2B
f2B
f2B
f2B
g8B
g8B
gB
g8B
h>B
h>B
h>B
h$B
h>B
h>B
iDB
jKB
jKB
jKB
jKB
j0B
kQB
kkB
kQB
kQB
kQB
k6B
kQB
kkB
kQB
l=B
lWB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
mCB
m]B
mCB
m]B
m]B
m]B
m]B
m]B
ncB
ncB
ncB
ncB
ncB
ncB
o�B
oiB
oOB
oiB
oiB
poB
poB
pUB
poB
poB
poB
poB
qvB
qvB
qvB
qvB
q�B
qvB
r|B
r|B
r|B
r|B
s�B
shB
s�B
s�B
s�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.41(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201811190039442018111900394420181119003944201811200030022018112000300220181120003002JA  ARFMdecpA19c                                                                20181114033615  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181113183648  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181113183649  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181113183650  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181113183650  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181113183650  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181113183650  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181113183650  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181113183651  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181113183651                      G�O�G�O�G�O�                JA  ARUP                                                                        20181113185516                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181114154313  CV  JULD            G�O�G�O�Fă                JM  ARCAJMQC2.0                                                                 20181118153944  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181118153944  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181119153002  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021521                      G�O�G�O�G�O�                