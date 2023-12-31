CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-02-15T12:36:20Z creation;2019-02-15T12:36:23Z conversion to V3.1;2019-12-23T06:06:55Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190215123620  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               }A   JA  I2_0675_125                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @اך[� 1   @ا�`��@7��i�B��c3n.��31   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�33A�33B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy�fDz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�33@љ�A��A'33AH��Ah��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�B��B
33B33B33B!��B*33B233B:33BB33BJ33BR33BZ33Bb33Bj33Br33Bz33B��B��B��B��B��B��B��fB��B��B��B��B��B��B�L�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfD #3D �3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D	#3D	�3D
#3D
�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D #3D �3D!#3D!�3D"#3D"�3D##3D#�3D$#3D$�3D%#3D%�3D&#3D&�3D'#3D'�3D(#3D(�3D)#3D)�3D*#3D*�3D+#3D+�3D,#3D,�3D-#3D-�3D.#3D.�3D/#3D/�3D0#3D0�3D1#3D1�3D2#3D2�3D3#3D3�3D4#3D4�3D5#3D5�3D6#3D6�3D7#3D7�3D8#3D8�3D9#3D9�3D:#3D:�3D;#3D;�3D<#3D<�3D=#3D=�3D>#3D>�3D?#3D?�3D@#3D@�3DA#3DA�3DB#3DB�3DC#3DC�3DD#3DD�3DE#3DE�3DF#3DF�3DG#3DG�3DH#3DH�3DI#3DI�3DJ#3DJ�3DK#3DK�3DL#3DL�3DM#3DM�3DN#3DN�3DO#3DO�3DP#3DP�3DQ#3DQ�3DR#3DR�3DS#3DS�3DT#3DT�3DU#3DU�3DV#3DV�3DW#3DW�3DX#3DX�3DY#3DY�3DZ#3DZ�3D[#3D[�3D\#3D\�3D]#3D]�3D^#3D^�3D_#3D_�3D`#3D`�3Da#3Da�3Db#3Db�3Dc#3Dc�3Dd#3Dd�3De#3De�3Df#3Df�3Dg#3Dg�3Dh#3Dh�3Di#3Di�3Dj#3Dj�3Dk#3Dk�3Dl#3Dl�3Dm#3Dm�3Dn#3Dn�3Do#3Do�3Dp#3Dp�3Dq#3Dq�3Dr#3Dr��Ds#3Ds�3Dt#3Dt�3Du#3Du�3Dv#3Dv�3Dw#3Dw�3Dx#3Dx�3Dy#3Dy��Dz#3Dz�3D{#3D{�3D|#3D|�3D}#3D}�3D~#3D~�3D#3D�3D��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D�D�њD��D�Q�DÑ�D�њD��D�Q�Dđ�D�њD��D�Q�Dő�D�њD��D�Q�DƑ�D�њD��D�Q�DǑ�D�њD��D�Q�Dȑ�D�њD��D�Q�Dɑ�D�њD��D�Q�Dʑ�D�њD��D�Q�Dˑ�D�њD��D�Q�D̑�D�њD��D�Q�D͑�D�њD��D�Q�DΑ�D�њD��D�Q�Dϑ�D�њD��D�Q�DБ�D�њD��D�Q�Dё�D�њD��D�Q�Dґ�D�њD��D�Q�Dӑ�D�њD��D�Q�Dԑ�D�њD��D�Q�DՑ�D�њD��D�Q�D֑�D�њD��D�Q�Dב�D�њD��D�Q�Dؑ�D�њD��D�Q�Dّ�D�њD��D�Q�Dڑ�D�њD��D�Q�Dۑ�D�њD��D�Q�Dܑ�D�њD��D�Q�Dݑ�D�њD��D�Q�Dޑ�D�њD��D�Q�Dߑ�D�њD��D�Q�D���D�њD��D�Q�DᑚD�њD��D�Q�D⑚D�њD��D�Q�D㑚D�њD��D�Q�D䑚D�њD��D�Q�D呚D�њD��D�Q�D摚D�њD��D�Q�D瑚D�њD��D�Q�D葚D�њD��D�Q�D鑚D�њD��D�Q�DꑚD�њD��D�Q�D둚D�њD��D�Q�D쑚D�њD��D�Q�D푚D�њD��D�Q�DD�њD��D�Q�DD�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D��D���D��D�Q�D�D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��
A��A��
A���A��
A��mA��mA��mA��yA��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�%A�%A�1A�1A�
=A�
=A�
=A�
=A�JA�JA�VA�VA�bA�bA�bA�1A�1A�A�  A�A���A��A��`A��+A�E�A��A�ffA�A�A�5?A��hA���A��A��A�r�A��PA���A���A�p�A��HA��;A���A�n�A�XA�ZA�dZA�hsA�A�A��A���A��A�ĜA��uA�~�A�  A�|�A��A��A���A�dZA�~�A��#A��uA�bNA�A�1'A��hA��A��^A�p�A�bA���A�jA��A��A���A�S�A���A��-A�bA��7A��A���A���A�C�A|�A~JA{|�AyoAu��As;dAp(�AmO�Aj�Ai��Ah�+Ae�AcAa�PA_S�A]&�A\(�AZ��AW��AVE�AT��ASXAQVAO�AL�yAJ��AI�^AI�AHr�AG��AG�AFJAC�wAC&�ACAB��AA�A?ƨA=t�A;��A:�`A9�wA85?A7t�A6�`A6{A5t�A3�;A2�!A21'A1��A1�7A1S�A1
=A0E�A/?}A-S�A,=qA+��A+�A*A)
=A'�mA'S�A%t�A#�A!�AAz�A�-A�HA�AXA �Ax�AK�AoA�AAbNAA��A\)AoA�\A�
A�A��Ar�A33A�-A��AĜA�+A��AO�A
=qA	�7A�\AA�`A��A�AA�PA�A�#AVA (�@�C�@�^5@���@�(�@�{@���@�t�@��@�5?@�bN@�\@�x�@�r�@�w@�l�@�R@�n�@�Z@�K�@�5?@�Q�@���@��@�S�@�^@�hs@�9@�j@�1'@�\)@�p�@�t�@ڰ!@�V@�1@�V@���@�x�@�x�@պ^@Չ7@�%@ԃ@�9X@��@Ӯ@Ӆ@�S�@�"�@љ�@�/@���@��@�x�@�1'@�v�@ǝ�@ă@°!@���@���@�@�@��9@�;d@�ff@��-@�7L@��j@���@��j@�(�@��@�5?@�&�@��u@��y@���@�V@�Q�@��@�K�@��!@�5?@���@�`B@���@��@��@�\)@�o@�ȴ@�v�@�^5@�E�@�{@��T@���@�&�@���@�1'@��@�ƨ@��@��P@�\)@�"�@��H@��+@�V@��@��T@��^@�x�@�X@�G�@���@���@��@�bN@��@�1@��m@���@�K�@�o@���@�ȴ@��!@��R@���@�-@��@���@���@�%@���@�1@��
@���@��P@�+@�@��!@�~�@�5?@�@���@�I�@�  @��w@�+@�
=@��@���@�^5@�E�@���@���@���@�=q@���@��-@��7@��@�`B@���@�(�@���@�\)@�;d@���@���@�n�@�ff@���@���@���@���@��h@��7@�hs@��@���@���@���@�%@�?}@��h@��@���@�b@��;@�1@���@���@�z�@�9X@�1@��;@���@��w@��F@���@�S�@�@���@��\@��\@��\@�V@�{@���@�O�@���@���@�Ĝ@���@�I�@�b@��;@��
@���@��@�\)@�+@��y@���@�v�@�^5@�V@�M�@�=q@�{@��-@��@�hs@���@��9@��@�r�@�bN@�Z@�A�@�b@��@�C�@�
=@��y@�^5@�@��@���@���@�`B@�&�@�V@��@��@�(�@� �@�b@���@���@���@��@��
@��F@��@�dZ@�"�@���@�v�@�M�@�=q@�{@��@�@��7@�G�@���@��/@��/@��9@�z�@�Q�@�1'@��@�@|�@K�@
=@~v�@~@}@}O�@|��@|j@|I�@{�m@{S�@z�!@zM�@z-@y�@y��@yhs@y�@x��@xr�@xQ�@x1'@w��@w;d@w�@w
=@v�R@v@u��@u�-@up�@uO�@u/@uV@t�@t�@t(�@s�
@st�@s33@r�@q�@q�^@q�7@q7L@p��@p�@pb@o�P@o+@n�R@nV@m�@m@m��@m�@m?}@l��@l�j@l�D@l9X@kƨ@k�@kC�@j�@j�!@jM�@i�@i7L@h��@h�`@h��@hbN@g�;@gK�@g;d@g+@f��@f��@f�+@f�+@fV@f$�@e��@e��@e�@d��@d�D@d9X@c�
@c�@cdZ@cC�@b�@b��@bM�@b�@a�^@ahs@a7L@a&�@`��@`�u@_�;@_\)@_;d@^��@^E�@]�T@]p�@\��@\�D@\j@\9X@[��@[�F@[��@[C�@Z�\@Z~�@Z�@Y�^@Yx�@Y7L@X�`@XbN@X1'@W�;@WK�@V�y@V�R@V$�@U��@T��@TI�@S��@S�F@SS�@So@So@S@R�@R�H@R�H@R�!@R-@Q�#@Q��@Qx�@Qhs@QX@Q&�@P�9@PbN@P  @OK�@N�@Nȴ@N�+@N5?@M�@L��@L��@L��@L�j@L��@L�D@Lj@L9X@L1@K��@K�m@K�@K33@Ko@J��@J^5@JJ@I��@I7L@HĜ@HbN@H1'@G�w@G�P@G�P@G|�@G
=@F5?@E�@E�@Ep�@E`B@E?}@D�@D��@D�j@D��@D�D@D�D@D�D@Dj@C�m@CdZ@C33@B=q@A�#@A�^@A�7@A�@@�u@@r�@@bN@@A�@?�@?�P@?|�@?�@>ȴ@>��@>ff@>5?@=@=`B@<��@<�@<Z@<1@<1@<1@;�
@;ƨ@;ƨ@;��@;�@;t�@;C�@;"�@;o@:��@:~�@:J@9x�@9�@8�`@8�`@8��@8�@8bN@8b@8  @7�w@7l�@7+@7�@7�@6��@6�y@6�@6��@6@5�T@5p�@4�/@4�@3��@3�m@3�
@3ƨ@3�@3dZ@3"�@2�@2��@2��@2�\@2^5@2M�@2=q@2�@1�@1�^@1��@1&�@0�u@0bN@0Q�@01'@0 �@0b@/�@/��@/�@/+@.��@.v�@.V@.@-�T@-@-�-@-�h@-O�@-V@,�/@,��@,�j@,j@,I�@,9X@,�@,�@+��@+�
@+��@+33@+o@+@*�H@*��@*~�@*n�@*^5@*M�@*=q@*J@)�@)�^@)x�@)G�@)�@(��@(��@(��@(r�@(Q�@(  @'�w@'|�@'\)@'�@&�@&��@&�+@&v�@&E�@&$�@%�@%��@%�@%O�@%V@$��@$�D@$Z@$9X@$1@#�F@#t�@#o@"�@"��@"��@"~�@"-@"J@!�^@!hs@ ��@ �9@ ��@ r�@ bN@  �@�;@�w@�P@K�@�@��@�+@ff@5?@$�@�@@�@O�@/@�@�@V@��@�@�D@�D@z�@(�@1@�F@dZ@S�@C�@33@�@�H@~�@J@�@��@hs@X@�@��@��@1'@�@��@�@��@l�@K�@;d@�@
=@�@��@ff@E�@E�@$�@�T@p�@/@��@�/@�/@��@�@(�@1@��@ƨ@�F@�@33@@��@�@��@��@��@hs@�@�`@�9@r�@Q�@1'@b@�@\)@K�@�@ȴ@�+@v�@ff@V@E�@$�@@�T@��@p�@O�@�@�@�j@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��
A��A��
A���A��
A��mA��mA��mA��yA��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�%A�%A�1A�1A�
=A�
=A�
=A�
=A�JA�JA�VA�VA�bA�bA�bA�1A�1A�A�  A�A���A��A��`A��+A�E�A��A�ffA�A�A�5?A��hA���A��A��A�r�A��PA���A���A�p�A��HA��;A���A�n�A�XA�ZA�dZA�hsA�A�A��A���A��A�ĜA��uA�~�A�  A�|�A��A��A���A�dZA�~�A��#A��uA�bNA�A�1'A��hA��A��^A�p�A�bA���A�jA��A��A���A�S�A���A��-A�bA��7A��A���A���A�C�A|�A~JA{|�AyoAu��As;dAp(�AmO�Aj�Ai��Ah�+Ae�AcAa�PA_S�A]&�A\(�AZ��AW��AVE�AT��ASXAQVAO�AL�yAJ��AI�^AI�AHr�AG��AG�AFJAC�wAC&�ACAB��AA�A?ƨA=t�A;��A:�`A9�wA85?A7t�A6�`A6{A5t�A3�;A2�!A21'A1��A1�7A1S�A1
=A0E�A/?}A-S�A,=qA+��A+�A*A)
=A'�mA'S�A%t�A#�A!�AAz�A�-A�HA�AXA �Ax�AK�AoA�AAbNAA��A\)AoA�\A�
A�A��Ar�A33A�-A��AĜA�+A��AO�A
=qA	�7A�\AA�`A��A�AA�PA�A�#AVA (�@�C�@�^5@���@�(�@�{@���@�t�@��@�5?@�bN@�\@�x�@�r�@�w@�l�@�R@�n�@�Z@�K�@�5?@�Q�@���@��@�S�@�^@�hs@�9@�j@�1'@�\)@�p�@�t�@ڰ!@�V@�1@�V@���@�x�@�x�@պ^@Չ7@�%@ԃ@�9X@��@Ӯ@Ӆ@�S�@�"�@љ�@�/@���@��@�x�@�1'@�v�@ǝ�@ă@°!@���@���@�@�@��9@�;d@�ff@��-@�7L@��j@���@��j@�(�@��@�5?@�&�@��u@��y@���@�V@�Q�@��@�K�@��!@�5?@���@�`B@���@��@��@�\)@�o@�ȴ@�v�@�^5@�E�@�{@��T@���@�&�@���@�1'@��@�ƨ@��@��P@�\)@�"�@��H@��+@�V@��@��T@��^@�x�@�X@�G�@���@���@��@�bN@��@�1@��m@���@�K�@�o@���@�ȴ@��!@��R@���@�-@��@���@���@�%@���@�1@��
@���@��P@�+@�@��!@�~�@�5?@�@���@�I�@�  @��w@�+@�
=@��@���@�^5@�E�@���@���@���@�=q@���@��-@��7@��@�`B@���@�(�@���@�\)@�;d@���@���@�n�@�ff@���@���@���@���@��h@��7@�hs@��@���@���@���@�%@�?}@��h@��@���@�b@��;@�1@���@���@�z�@�9X@�1@��;@���@��w@��F@���@�S�@�@���@��\@��\@��\@�V@�{@���@�O�@���@���@�Ĝ@���@�I�@�b@��;@��
@���@��@�\)@�+@��y@���@�v�@�^5@�V@�M�@�=q@�{@��-@��@�hs@���@��9@��@�r�@�bN@�Z@�A�@�b@��@�C�@�
=@��y@�^5@�@��@���@���@�`B@�&�@�V@��@��@�(�@� �@�b@���@���@���@��@��
@��F@��@�dZ@�"�@���@�v�@�M�@�=q@�{@��@�@��7@�G�@���@��/@��/@��9@�z�@�Q�@�1'@��@�@|�@K�@
=@~v�@~@}@}O�@|��@|j@|I�@{�m@{S�@z�!@zM�@z-@y�@y��@yhs@y�@x��@xr�@xQ�@x1'@w��@w;d@w�@w
=@v�R@v@u��@u�-@up�@uO�@u/@uV@t�@t�@t(�@s�
@st�@s33@r�@q�@q�^@q�7@q7L@p��@p�@pb@o�P@o+@n�R@nV@m�@m@m��@m�@m?}@l��@l�j@l�D@l9X@kƨ@k�@kC�@j�@j�!@jM�@i�@i7L@h��@h�`@h��@hbN@g�;@gK�@g;d@g+@f��@f��@f�+@f�+@fV@f$�@e��@e��@e�@d��@d�D@d9X@c�
@c�@cdZ@cC�@b�@b��@bM�@b�@a�^@ahs@a7L@a&�@`��@`�u@_�;@_\)@_;d@^��@^E�@]�T@]p�@\��@\�D@\j@\9X@[��@[�F@[��@[C�@Z�\@Z~�@Z�@Y�^@Yx�@Y7L@X�`@XbN@X1'@W�;@WK�@V�y@V�R@V$�@U��@T��@TI�@S��@S�F@SS�@So@So@S@R�@R�H@R�H@R�!@R-@Q�#@Q��@Qx�@Qhs@QX@Q&�@P�9@PbN@P  @OK�@N�@Nȴ@N�+@N5?@M�@L��@L��@L��@L�j@L��@L�D@Lj@L9X@L1@K��@K�m@K�@K33@Ko@J��@J^5@JJ@I��@I7L@HĜ@HbN@H1'@G�w@G�P@G�P@G|�@G
=@F5?@E�@E�@Ep�@E`B@E?}@D�@D��@D�j@D��@D�D@D�D@D�D@Dj@C�m@CdZ@C33@B=q@A�#@A�^@A�7@A�@@�u@@r�@@bN@@A�@?�@?�P@?|�@?�@>ȴ@>��@>ff@>5?@=@=`B@<��@<�@<Z@<1@<1@<1@;�
@;ƨ@;ƨ@;��@;�@;t�@;C�@;"�@;o@:��@:~�@:J@9x�@9�@8�`@8�`@8��@8�@8bN@8b@8  @7�w@7l�@7+@7�@7�@6��@6�y@6�@6��@6@5�T@5p�@4�/@4�@3��@3�m@3�
@3ƨ@3�@3dZ@3"�@2�@2��@2��@2�\@2^5@2M�@2=q@2�@1�@1�^@1��@1&�@0�u@0bN@0Q�@01'@0 �@0b@/�@/��@/�@/+@.��@.v�@.V@.@-�T@-@-�-@-�h@-O�@-V@,�/@,��@,�j@,j@,I�@,9X@,�@,�@+��@+�
@+��@+33@+o@+@*�H@*��@*~�@*n�@*^5@*M�@*=q@*J@)�@)�^@)x�@)G�@)�@(��@(��@(��@(r�@(Q�@(  @'�w@'|�@'\)@'�@&�@&��@&�+@&v�@&E�@&$�@%�@%��@%�@%O�@%V@$��@$�D@$Z@$9X@$1@#�F@#t�@#o@"�@"��@"��@"~�@"-@"J@!�^@!hs@ ��@ �9@ ��@ r�@ bN@  �@�;@�w@�P@K�@�@��@�+@ff@5?@$�@�@@�@O�@/@�@�@V@��@�@�D@�D@z�@(�@1@�F@dZ@S�@C�@33@�@�H@~�@J@�@��@hs@X@�@��@��@1'@�@��@�@��@l�@K�@;d@�@
=@�@��@ff@E�@E�@$�@�T@p�@/@��@�/@�/@��@�@(�@1@��@ƨ@�F@�@33@@��@�@��@��@��@hs@�@�`@�9@r�@Q�@1'@b@�@\)@K�@�@ȴ@�+@v�@ff@V@E�@$�@@�T@��@p�@O�@�@�@�j@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  BB  BBBBPBuB�B'�BG�B`BBaHBcTBdZBcTBbNBF�B33B#�B �B/B0!B0!B7LBH�BL�BR�B]/BbNBaHB`BB`BBn�Br�Br�Bo�BiyBbNBQ�BB��B�}B��B��B��B��B�+Bw�B^5B5?B#�B�B�BbB1B
��B
�B
�sB
��B
�^B
�=B
VB
)�B
�B
�B
VB
+B	��B	��B	�B	�
B	ÖB	�-B	��B	�1B	� B	w�B	dZB	S�B	G�B	;dB	-B	#�B	�B	
=B��B��B�B�NB��B��B��B��B��BǮBŢB��B�qB�FB�9B�3B�-B�!B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�bB�bB�bB�PB�=B�B�B~�B{�Bw�Br�Bm�BjBaHBXBS�BI�BH�BH�BI�BH�BG�BD�BB�BA�BA�B@�B?}B>wB<jB<jB<jB;dB9XB:^B7LB6FB5?B6FB5?B2-B2-B1'B2-B1'B0!B/B/B.B-B,B+B+B,B)�B)�B)�B)�B(�B'�B&�B'�B&�B&�B&�B&�B%�B'�B'�B&�B'�B'�B'�B&�B&�B(�B'�B'�B(�B(�B(�B,B+B+B,B,B+B,B/B1'B1'B33B33B7LB8RB<jBF�BL�BO�BXB^5B_;B`BBbNBcTBcTBdZBgmBgmBffBgmBgmBjBl�BhsBl�Br�Bv�Bw�Bw�Br�Br�Bs�Bt�Bw�Bx�By�B�B�+B�1B�1B�=B�7B�JB�PB�\B�bB�{B��B��B��B��B��B��B��B��B�B�B�B�!B�'B�3B�9B�FB�XB�dB�}BÖBƨBȴBɺB��B��B��B��B��B�B�B�/B�;B�HB�ZB�fB�yB�B�B�B��B��B��B��B��B	B	%B	
=B	DB	PB	VB	\B	hB	oB	{B	�B	�B	�B	�B	 �B	 �B	 �B	!�B	"�B	$�B	%�B	(�B	+B	1'B	6FB	:^B	=qB	@�B	@�B	@�B	C�B	D�B	G�B	L�B	O�B	P�B	R�B	S�B	W
B	XB	ZB	]/B	_;B	cTB	dZB	gmB	iyB	n�B	q�B	s�B	u�B	z�B	|�B	{�B	|�B	}�B	�B	�B	�B	�B	�=B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�9B	�9B	�?B	�FB	�RB	�RB	�RB	�LB	�RB	�^B	�dB	�jB	�}B	��B	��B	ÖB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�5B	�HB	�NB	�NB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B

=B

=B

=B

=B

=B
DB
DB
JB
JB
PB
PB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
bB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
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
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
6FB
6FB
7LB
7LB
7LB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
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
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
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
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
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
W
B
XB
XB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
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
\)B
\)B
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
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
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
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
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
l�B
l�B
l�B
l�B
l�B
l�B
m�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B �B��B�B�B�BB@BkB'�BGzB`BaBc Bd&Bc BbBFtB2�B#�B �B.�B/�B/�B7BH�BL�BR�B\�BbBaB`B`BncBr|Br|BoiBiDBbBQ�B�BʌB�HB��B��B��B�YB��Bw�B^B5B#�BkBSB.B�B
��B
�B
�>B
��B
�*B
�	B
U�B
)�B
qB
YB
"B
�B	��B	��B	�cB	��B	�aB	��B	��B	��B	�B	w�B	d&B	S�B	GzB	;0B	,�B	#�B	qB		�B��B��B�QB�B��BϫB͟B˒BʌB�zB�mB�UB�<B��B��B��B��B��B��B��B��B��B��B��B��B�jB�~B�kB�eB�@B�B�.B�.B�.B�B�B�	B��B��B~�B{�Bw�Br|Bm]Bj0BaBW�BS�BIlBH�BH�BI�BHfBGzBDMBBABAUBAUB@OB?HB>(B<6B<6B<B;B9$B:B7B6B5B5�B5B1�B1�B0�B1�B0�B/�B.�B.�B-�B,�B+�B*�B*�B+�B)�B)�B)�B)�B(�B'�B&�B'�B&�B&�B&�B&�B%�B'�B'�B&�B'�B'�B'�B&�B&�B(�B'�B'�B(�B(�B(�B+�B*�B*�B+�B+�B*�B+�B.�B0�B0�B2�B2�B6�B8B<BFtBL~BO�BW�B^B_B`BbBcBcBdBg8Bg8Bf2BgBgBj0BlWBh$BlWBraBvzBw�Bw�BraBr|BshBt�Bw�Bx�By�B��B��B��B��B��B��B��B�B�B�.B�,B�?B�?B�WB��B�|B��B��B��B��B��B��B��B��B��B��B��B�$B�0B�.B�aB�YB�fB�lB�~B̈́BЗBҽBԯB��B��B��B��B��B�B�B�DB�=B�OB�hB�tB��B��B��B��B	�B	�B		�B	
�B	B	"B	B	B	 B	,B	?B	QB	QB	�B	 �B	 vB	 vB	!|B	"�B	$�B	%�B	(�B	*�B	0�B	5�B	:B	="B	@OB	@4B	@4B	CGB	DMB	G_B	L~B	O�B	P�B	R�B	S�B	V�B	W�B	Y�B	\�B	^�B	cB	dB	gB	i*B	ncB	q[B	shB	utB	z�B	|�B	{�B	|�B	}�B	��B	��B	��B	��B	��B	�B	�4B	�MB	�eB	�KB	�WB	�]B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	�B	�B	��B	�B	�*B	�B	�B	�HB	�4B	�;B	�GB	�mB	�_B	�fB	ɆB	ʌB	�xB	�~B	̈́B	ΥB	ϑB	ЗB	ЗB	ңB	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�&B	�&B	�B	�B	�B	�B	�B	�>B	�0B	�QB	�6B	�6B	�6B	�QB	�6B	�6B	�6B	�WB	�=B	�=B	�CB	�cB	�iB	�OB	�iB	�oB	�UB	�aB	�|B	�aB	�aB	�aB	�aB	�hB	�hB	�B	�nB	��B	��B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

	B

	B

	B
	�B
	�B

�B

�B
�B
B
B
B
B
B
B
B
B
(B
B
B
.B
B
B
B
B
4B
:B
 B
&B
&B
@B
&B
FB
,B
2B
MB
2B
2B
MB
2B
2B
9B
9B
SB
SB
?B
?B
?B
EB
_B
EB
EB
_B
EB
KB
KB
KB
QB
kB
kB
QB
QB
WB
]B
]B
xB
]B
dB
dB
�B
pB
�B
pB
pB
 vB
 vB
 �B
 vB
!|B
!|B
!|B
"�B
"�B
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
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
3�B
4�B
4�B
5�B
6B
5�B
5�B
6B
5�B
6B
5�B
6�B
6�B
6�B
5�B
5�B
7B
7B
7B
9	B
9$B
9	B
9	B
9	B
:*B
:B
:B
:*B
:B
;B
;0B
;B
;B
;B
<B
<B
<B
<6B
=<B
="B
=<B
>(B
>BB
>(B
?.B
?.B
>(B
?.B
?HB
?.B
?.B
?.B
?.B
?.B
?.B
@4B
@OB
AUB
AUB
A;B
A;B
A;B
BAB
BAB
BAB
CGB
CGB
CGB
CaB
CGB
CaB
CGB
CaB
CGB
DgB
DMB
DMB
ESB
ESB
FYB
FtB
FYB
FtB
FYB
FtB
G_B
G_B
G_B
GzB
G_B
H�B
HfB
HfB
HfB
HfB
HfB
H�B
HfB
I�B
I�B
IlB
JrB
JrB
JrB
J�B
JrB
JrB
JrB
KxB
K�B
K�B
KxB
K�B
L�B
L~B
L~B
L~B
L~B
L~B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
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
R�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
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
[�B
[�B
\�B
\�B
\�B
\�B
^B
]�B
^B
]�B
^B
]�B
]�B
^B
]�B
^�B
^�B
_B
_�B
_�B
`B
`B
_�B
`�B
`�B
`�B
`�B
bB
a�B
a�B
bB
a�B
bB
a�B
cB
cB
cB
cB
cB
c B
cB
dB
dB
dB
d&B
dB
dB
dB
eB
eB
eB
eB
f2B
f2B
fB
fB
gB
g8B
gB
h$B
h>B
h$B
h$B
h$B
i*B
iDB
i*B
iDB
i*B
j0B
j0B
j0B
j0B
jKB
k6B
k6B
kQB
k6B
k6B
k6B
k6B
k6B
l=B
l=B
l=B
l=B
lWB
lWB
m]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.55(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201902210038352019022100383520190221003835201902220021082019022200210820190222002108JA  ARFMdecpA19c                                                                20190215213619  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190215123620  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190215123621  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190215123622  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190215123622  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190215123622  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190215123623  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190215123623  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190215123623  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190215123623                      G�O�G�O�G�O�                JA  ARUP                                                                        20190215125642                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190215153637  CV  JULD            G�O�G�O�F�>�                JM  ARCAJMQC2.0                                                                 20190220153835  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190220153835  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190221152108  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                