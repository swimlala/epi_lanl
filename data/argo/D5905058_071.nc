CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-23T07:09:01Z creation;2018-07-23T07:09:06Z conversion to V3.1;2019-12-23T06:19:28Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �P   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �P   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �P   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �P   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20180723070901  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               GA   JA  I2_0675_071                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�n0>�u 1   @�n0��� @91a��e��c(��f�B1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�3D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�C3D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�  @љ�A��A(��AJffAh��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�B33B
33B33B33B"33B*33B233B:33BB33BJ33BR33BZ33Bb33Bj33Br33Bz33B��B��B��B��B��B��fB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn�fCp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�9�C�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfC�FfD #3D �3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D	#3D	�3D
#3D
�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D#3D�3D #3D �3D!#3D!�3D"#3D"�3D##3D#�3D$#3D$�3D%#3D%�3D&#3D&�3D'#3D'�3D(#3D(�3D)#3D)�3D*#3D*�3D+#3D+�3D,#3D,�3D-#3D-�3D.#3D.�3D/#3D/�3D0#3D0�3D1#3D1�3D2#3D2�3D3#3D3�3D4#3D4�3D5#3D5�3D6#3D6�3D7#3D7�3D8#3D8�3D9#3D9�3D:#3D:�3D;#3D;�3D<#3D<�3D=#3D=�3D>#3D>�3D?#3D?�3D@#3D@�3DA#3DA�3DB#3DB�3DC#3DC�3DD#3DD�3DE#3DE�3DF#3DF�3DG#3DG�3DH#3DH�3DI#3DI�3DJ#3DJ�3DK#3DK�3DL#3DL�3DM#3DM�3DN#3DN�3DO#3DO�3DP#3DP�3DQ#3DQ�3DR#3DR�3DS#3DS�3DT#3DT�3DU#3DU�3DV#3DV�3DW#3DW�3DX#3DX�3DY#3DY�3DZ#3DZ�3D[#3D[�3D\#3D\�3D]#3D]�3D^#3D^�3D_#3D_�3D`#3D`�3Da#3Da�3Db#3Db�3Dc#3Dc�3Dd#3Dd�3De#3De�3Df#3Df�3Dg#3Dg�3Dh#3Dh�3Di#3Di�3Dj#3Dj�3Dk#3Dk�3Dl#3Dl�3Dm#3Dm�3Dn#3Dn�3Do#3Do�3Dp#3Dp�3Dq#3Dq�3Dr#3Dr�3Ds#3Ds�3Dt#3Dt�3Du#3Du�3Dv#3Dv�3Dw#3Dw�3Dx#3Dx�3Dy#3Dy�3Dz#3Dz�3D{#3D{�3D|#3D|�3D}#3D}�3D~#3D~�3D#3D�3D��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D�D�њD��D�Q�DÑ�D�њD��D�Q�Dđ�D�њD��D�Q�Dő�D�њD��D�Q�DƑ�D�њD��D�Q�DǑ�D�њD��D�Q�Dȑ�D�њD��D�Q�Dɑ�D�њD��D�Q�Dʑ�D�њD��D�Q�Dˑ�D�њD��D�Q�D̑�D�њD��D�Q�D͑�D�њD��D�Q�DΑ�D�њD��D�Q�Dϑ�D�њD��D�Q�DБ�D�њD��D�Q�Dё�D�њD��D�Q�Dґ�D�њD��D�Q�Dӑ�D�њD��D�Q�Dԑ�D�њD��D�Q�DՑ�D�њD��D�Q�D֑�D�њD��D�Q�Dב�D�њD��D�Q�Dؑ�D�њD��D�Q�Dّ�D�њD��D�Q�Dڑ�D�њD��D�Q�Dۑ�D�њD��D�Q�Dܑ�D�њD��D�Q�Dݑ�D�њD��D�Q�Dޑ�D�њD��D�Q�Dߑ�D�њD��D�Q�D���D�њD��D�Q�DᑚD�њD��D�Q�D⑚D�њD��D�Q�D㑚D�њD��D�Q�D䑚D�њD��D�Q�D呚D�њD��D�Q�D摚D�њD��D�Q�D瑚D�њD��D�Q�D葚D�њD��D�Q�D鑚D�њD��D�Q�DꑚD�њD��D�Q�D둚D�њD��D�Q�D쑚D�њD��D�Q�D푚D�њD��D�Q�DD�њD��D�Q�DD�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D�D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�T�D���D�њD��D�T�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AƓuAƓuAƓuAƓuAƓuAƓuAƕ�Aƕ�Aƕ�Aƕ�Aƕ�AƗ�AƗ�A¥�A���A���A�dZA�1'A��yA��DA��mA�&�A�1A�VA���A��!A��RA��A��DA��;A���A�33A��#A�9XA�ffA�
=A���A��A�"�A�dZA�  A���A���A�\)A�1'A�{A�oA���A��A��A��;A��-A���A�x�A�Q�A�(�A��TA��!A�hsA�  A�|�A��A���A�/A�~�A���A�^5A�9XA��#A���A�bA��A�33A�ZA��PA�x�A��PA��A��^A�z�A��!A�x�A��A�n�A�ĜA���A��yA�bA�;dA�VA�A�A�9XA��A�?}A�r�A�C�A���A��HA���A��A�VA�v�A���A��yAC�A~�A~-A}p�A{�AxĜAuO�Ar�HAoAm`BAmVAl=qAj�Ai��Ag;dAdbAbVAat�A_�7A^��A]t�A\  AYG�AXA�AX  AWAVjAT�ASS�AQK�AOAN��AM�;AL�AL�AK�AKt�AJ�!AJ(�AI�AIƨAH��AG��AFr�AD��AC;dABv�AA��A@-A?
=A>=qA=t�A<{A:��A8JA6ȴA69XA5�TA5�A4�!A2�A2bA1��A0v�A09XA01'A/dZA.A,��A+��A*��A(��A&�A%�#A%�A%oA$�A$�A#�A!l�A!/A ��A?}A�A �AA��A9XA��A�AbAƨA7LA��A�A��A��A�A��A�`AbNA$�A�FA\)A;dA��AQ�A��AC�A��A  A�;A�FA�A�9A�DA��At�A�A�A
=A-A ��A E�A �@�v�@���@��P@�7L@��T@�z�@�1@�\)@��@�E�@�1'@�!@��@��@�M�@��@�/@�1@�E�@��T@��@��@�K�@�n�@��@�Ĝ@��m@ߍP@݉7@ڸR@�~�@���@��@���@؃@�1@�t�@�=q@���@�(�@���@ӕ�@ҸR@�%@�l�@��@ͩ�@�/@���@�r�@�+@�@ȼj@�bN@�t�@�~�@őh@��@�Ĝ@�\)@���@��-@���@�r�@�A�@��@���@���@�l�@�ff@��@�Ĝ@���@�`B@���@���@���@��@���@���@��@���@�9X@���@�l�@���@��D@�5?@���@��;@�t�@���@�-@��7@�7L@��@���@��`@�z�@�S�@�"�@�"�@�+@��@�n�@��@���@�/@��`@��@��u@�9X@�ƨ@��@���@���@��T@��7@��@�G�@�O�@���@�I�@�l�@�\)@��@�ff@�V@���@�?}@��@��9@�bN@�I�@�9X@�1'@��@�9X@��@��w@��w@���@��@���@�M�@�5?@�-@��^@��h@��@�?}@�%@��9@�z�@���@�Ĝ@� �@��w@���@���@�t�@�"�@���@�E�@��@���@�G�@��@���@�r�@�A�@�b@��;@�ƨ@���@�t�@�S�@�
=@�ȴ@��!@���@��\@�v�@�=q@�{@��@���@��-@�hs@�&�@���@��/@�Ĝ@���@�I�@��@�  @��m@��w@�dZ@�"�@��y@�ȴ@�ȴ@���@�ff@�J@���@���@�p�@�?}@��@��@��9@�Ĝ@���@�1@�1@�b@��;@��F@��@�S�@�;d@�33@�"�@��@��R@��R@�~�@�O�@�?}@�%@�Ĝ@�bN@�I�@�1'@�(�@�1'@�1'@��m@��m@�1@�  @�ƨ@��;@��;@�ƨ@��F@��@��@���@���@���@�ff@��#@�x�@�X@���@���@�Ĝ@�r�@�1'@��;@��@�1@�ƨ@��w@���@�1@�b@��@��@��@���@���@��@��@�\)@�S�@�33@�o@�@��@���@�E�@�@��@��T@�@���@�x�@��@��@�A�@��@�(�@�Z@�bN@�r�@��D@��@�9X@�1@�w@|�@K�@+@~�y@~ff@}��@}p�@|��@|��@|��@|I�@{ƨ@{C�@z��@z�\@z�@y�^@yhs@y%@x�@xA�@xb@x  @w�;@w��@w;d@vV@up�@uO�@up�@uV@t��@tz�@tz�@tj@t9X@t�@t1@s�m@r�H@q��@qX@pA�@ol�@nȴ@n@n$�@n$�@m�-@l�@lj@l�@k��@l�@k��@j~�@jJ@i�@i�#@i��@i��@iX@i%@h�`@h�9@hA�@g�@g+@f�y@fȴ@f�R@fv�@fE�@f5?@e�@d�@c��@c�F@b�!@b=q@ax�@` �@_�;@_�@_l�@_;d@^�@^�+@^$�@]��@]�@]O�@]�@\�@\�@\j@\Z@\(�@\1@[��@[�F@[�@[S�@Z�@Z��@Z^5@ZM�@Z-@Z�@ZJ@Y��@Y�#@Y�7@Yhs@Y7L@X��@XA�@W�w@W�@V�+@VE�@V$�@V@U@Up�@U�@T��@T�@T�D@T9X@S��@S�@SdZ@S"�@R��@Rn�@Q�#@Q��@Q&�@P��@P1'@P  @O�;@O�w@O�P@O�@Nv�@N{@M@M�@L�@L�D@K�
@KS�@K@J��@J��@J�\@J�\@Jn�@J-@I�#@I�7@I%@HĜ@HĜ@H��@HbN@H1'@G�@G;d@G;d@G;d@F��@F�@F��@FV@F$�@F$�@E�@E��@EV@DZ@C��@CC�@Co@B�@B��@B�\@B=q@BJ@A��@AX@AG�@AG�@A7L@A7L@A&�@@��@@1'@@  @@  @?�@?�;@?+@>ff@>@=?}@<��@<��@<�D@<z�@<I�@;��@;33@:�!@:-@9�@9�#@9X@8��@8�u@8A�@8  @7��@7�@7�P@7l�@7+@7
=@6�R@6v�@6V@6V@6V@6E�@65?@5�@6@5�T@5��@5V@4��@4I�@41@3ƨ@3��@3S�@3o@2��@2�\@2^5@2�@1�^@1X@1�@0��@0��@0��@01'@0 �@/�w@.��@.v�@.{@-�@-�T@-�T@-�T@-�T@-�T@-p�@,��@,�D@,I�@,1@+�@+o@*��@*-@*J@*-@*-@)X@)�@)%@(�`@(�9@(�u@(bN@(A�@(  @(A�@(A�@'��@'�P@'\)@';d@'+@'�@&�@&�+@&E�@%�T@%��@&@%�@&@&@%��@%�-@%�h@%p�@%p�@%`B@%�@$�j@$�@$1@#dZ@#C�@#S�@#t�@#dZ@#dZ@#33@#"�@#o@"�@"�H@"��@"��@"�\@"^5@"-@"J@!��@!�@!�#@!��@!X@ ��@ �u@ r�@ bN@ 1'@   @\)@
=@
=@�@��@�+@V@5?@@��@�-@p�@?}@�@�@�@�/@�j@z�@j@Z@��@�@dZ@C�@33@o@@�!@-@�#@x�@X@7L@Ĝ@�u@r�@Q�@ �@�@+@��@�R@��@ff@5?@$�@��@p�@?}@?}@�@V@��@��@�j@�@��@z�@j@9X@�@�@�@��@��@dZ@o@�H@�!@�\@��@��@x�@G�@7L@&�@&�@&�@�@%@�`@��@Ĝ@r�@A�@A�@1'@ �@b@ �@b@  @  @  @�@�w@�P@|�@\)@K�@+@
=@�y@��@ff@ff@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AƓuAƓuAƓuAƓuAƓuAƓuAƕ�Aƕ�Aƕ�Aƕ�Aƕ�AƗ�AƗ�A¥�A���A���A�dZA�1'A��yA��DA��mA�&�A�1A�VA���A��!A��RA��A��DA��;A���A�33A��#A�9XA�ffA�
=A���A��A�"�A�dZA�  A���A���A�\)A�1'A�{A�oA���A��A��A��;A��-A���A�x�A�Q�A�(�A��TA��!A�hsA�  A�|�A��A���A�/A�~�A���A�^5A�9XA��#A���A�bA��A�33A�ZA��PA�x�A��PA��A��^A�z�A��!A�x�A��A�n�A�ĜA���A��yA�bA�;dA�VA�A�A�9XA��A�?}A�r�A�C�A���A��HA���A��A�VA�v�A���A��yAC�A~�A~-A}p�A{�AxĜAuO�Ar�HAoAm`BAmVAl=qAj�Ai��Ag;dAdbAbVAat�A_�7A^��A]t�A\  AYG�AXA�AX  AWAVjAT�ASS�AQK�AOAN��AM�;AL�AL�AK�AKt�AJ�!AJ(�AI�AIƨAH��AG��AFr�AD��AC;dABv�AA��A@-A?
=A>=qA=t�A<{A:��A8JA6ȴA69XA5�TA5�A4�!A2�A2bA1��A0v�A09XA01'A/dZA.A,��A+��A*��A(��A&�A%�#A%�A%oA$�A$�A#�A!l�A!/A ��A?}A�A �AA��A9XA��A�AbAƨA7LA��A�A��A��A�A��A�`AbNA$�A�FA\)A;dA��AQ�A��AC�A��A  A�;A�FA�A�9A�DA��At�A�A�A
=A-A ��A E�A �@�v�@���@��P@�7L@��T@�z�@�1@�\)@��@�E�@�1'@�!@��@��@�M�@��@�/@�1@�E�@��T@��@��@�K�@�n�@��@�Ĝ@��m@ߍP@݉7@ڸR@�~�@���@��@���@؃@�1@�t�@�=q@���@�(�@���@ӕ�@ҸR@�%@�l�@��@ͩ�@�/@���@�r�@�+@�@ȼj@�bN@�t�@�~�@őh@��@�Ĝ@�\)@���@��-@���@�r�@�A�@��@���@���@�l�@�ff@��@�Ĝ@���@�`B@���@���@���@��@���@���@��@���@�9X@���@�l�@���@��D@�5?@���@��;@�t�@���@�-@��7@�7L@��@���@��`@�z�@�S�@�"�@�"�@�+@��@�n�@��@���@�/@��`@��@��u@�9X@�ƨ@��@���@���@��T@��7@��@�G�@�O�@���@�I�@�l�@�\)@��@�ff@�V@���@�?}@��@��9@�bN@�I�@�9X@�1'@��@�9X@��@��w@��w@���@��@���@�M�@�5?@�-@��^@��h@��@�?}@�%@��9@�z�@���@�Ĝ@� �@��w@���@���@�t�@�"�@���@�E�@��@���@�G�@��@���@�r�@�A�@�b@��;@�ƨ@���@�t�@�S�@�
=@�ȴ@��!@���@��\@�v�@�=q@�{@��@���@��-@�hs@�&�@���@��/@�Ĝ@���@�I�@��@�  @��m@��w@�dZ@�"�@��y@�ȴ@�ȴ@���@�ff@�J@���@���@�p�@�?}@��@��@��9@�Ĝ@���@�1@�1@�b@��;@��F@��@�S�@�;d@�33@�"�@��@��R@��R@�~�@�O�@�?}@�%@�Ĝ@�bN@�I�@�1'@�(�@�1'@�1'@��m@��m@�1@�  @�ƨ@��;@��;@�ƨ@��F@��@��@���@���@���@�ff@��#@�x�@�X@���@���@�Ĝ@�r�@�1'@��;@��@�1@�ƨ@��w@���@�1@�b@��@��@��@���@���@��@��@�\)@�S�@�33@�o@�@��@���@�E�@�@��@��T@�@���@�x�@��@��@�A�@��@�(�@�Z@�bN@�r�@��D@��@�9X@�1@�w@|�@K�@+@~�y@~ff@}��@}p�@|��@|��@|��@|I�@{ƨ@{C�@z��@z�\@z�@y�^@yhs@y%@x�@xA�@xb@x  @w�;@w��@w;d@vV@up�@uO�@up�@uV@t��@tz�@tz�@tj@t9X@t�@t1@s�m@r�H@q��@qX@pA�@ol�@nȴ@n@n$�@n$�@m�-@l�@lj@l�@k��@l�@k��@j~�@jJ@i�@i�#@i��@i��@iX@i%@h�`@h�9@hA�@g�@g+@f�y@fȴ@f�R@fv�@fE�@f5?@e�@d�@c��@c�F@b�!@b=q@ax�@` �@_�;@_�@_l�@_;d@^�@^�+@^$�@]��@]�@]O�@]�@\�@\�@\j@\Z@\(�@\1@[��@[�F@[�@[S�@Z�@Z��@Z^5@ZM�@Z-@Z�@ZJ@Y��@Y�#@Y�7@Yhs@Y7L@X��@XA�@W�w@W�@V�+@VE�@V$�@V@U@Up�@U�@T��@T�@T�D@T9X@S��@S�@SdZ@S"�@R��@Rn�@Q�#@Q��@Q&�@P��@P1'@P  @O�;@O�w@O�P@O�@Nv�@N{@M@M�@L�@L�D@K�
@KS�@K@J��@J��@J�\@J�\@Jn�@J-@I�#@I�7@I%@HĜ@HĜ@H��@HbN@H1'@G�@G;d@G;d@G;d@F��@F�@F��@FV@F$�@F$�@E�@E��@EV@DZ@C��@CC�@Co@B�@B��@B�\@B=q@BJ@A��@AX@AG�@AG�@A7L@A7L@A&�@@��@@1'@@  @@  @?�@?�;@?+@>ff@>@=?}@<��@<��@<�D@<z�@<I�@;��@;33@:�!@:-@9�@9�#@9X@8��@8�u@8A�@8  @7��@7�@7�P@7l�@7+@7
=@6�R@6v�@6V@6V@6V@6E�@65?@5�@6@5�T@5��@5V@4��@4I�@41@3ƨ@3��@3S�@3o@2��@2�\@2^5@2�@1�^@1X@1�@0��@0��@0��@01'@0 �@/�w@.��@.v�@.{@-�@-�T@-�T@-�T@-�T@-�T@-p�@,��@,�D@,I�@,1@+�@+o@*��@*-@*J@*-@*-@)X@)�@)%@(�`@(�9@(�u@(bN@(A�@(  @(A�@(A�@'��@'�P@'\)@';d@'+@'�@&�@&�+@&E�@%�T@%��@&@%�@&@&@%��@%�-@%�h@%p�@%p�@%`B@%�@$�j@$�@$1@#dZ@#C�@#S�@#t�@#dZ@#dZ@#33@#"�@#o@"�@"�H@"��@"��@"�\@"^5@"-@"J@!��@!�@!�#@!��@!X@ ��@ �u@ r�@ bN@ 1'@   @\)@
=@
=@�@��@�+@V@5?@@��@�-@p�@?}@�@�@�@�/@�j@z�@j@Z@��@�@dZ@C�@33@o@@�!@-@�#@x�@X@7L@Ĝ@�u@r�@Q�@ �@�@+@��@�R@��@ff@5?@$�@��@p�@?}@?}@�@V@��@��@�j@�@��@z�@j@9X@�@�@�@��@��@dZ@o@�H@�!@�\@��@��@x�@G�@7L@&�@&�@&�@�@%@�`@��@Ĝ@r�@A�@A�@1'@ �@b@ �@b@  @  @  @�@�w@�P@|�@\)@K�@+@
=@�y@��@ff@ff@ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B@�B@�B@�B?}B@�B@�B@�B@�B@�B@�B@�B?}B;dB]/Bs�Bz�B|�B�B�VB�{B��B��B��B��B��B��B��B��B��B��B��B��B�-B�!B�B��B�VB�VB�\B�hB��B��B��B�9B�-B�-B�-B�9B�LBŢB��B��B��B��B��B��B��BǮB��B�^B�3B��B��B��B�Bt�Bk�BiyBdZBN�B+B��B�B�;B��BuBB��B�B�sB�/BɺBÖB�^B�!B��B{�Bl�BcTBP�B<jB,B�B1B
�B
�B
��B
��B
�B
��B
��B
��B
�+B
{�B
hsB
`BB
]/B
W
B
I�B
6FB
�B
+B	��B	�/B	�#B	��B	ɺB	��B	�B	��B	�JB	�B	x�B	r�B	n�B	bNB	O�B	I�B	J�B	K�B	G�B	<jB	33B	'�B	�B	�B	bB	JB	\B	\B	PB		7B	B	B	B��B��B�B�fB�NB�5B�B��B��BɺBĜB�^B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB�7B�B}�Bu�Bp�Bk�Bl�Bm�Bl�BjBjBffBffBgmB`BB^5B^5B]/BW
BR�BP�BN�BK�BI�BG�BC�B=qB9XB6FB5?B5?B2-B1'B/B/B.B-B-B-B+B+B)�B'�B'�B&�B&�B&�B%�B%�B$�B$�B#�B"�B"�B"�B!�B �B"�B!�B �B"�B#�B"�B"�B"�B"�B"�B&�B%�B&�B#�B"�B!�B#�B%�B#�B#�B#�B$�B'�B'�B&�B)�B)�B)�B0!B49B49B7LB7LB7LB8RB8RB8RB8RB;dB:^B;dB<jB=qB>wB<jB?}BB�BB�BC�BF�BF�BE�BF�BO�BR�BQ�BS�BXBW
B^5B`BBbNBe`BiyBiyBiyBjBjBn�Bp�Bq�Bq�Br�Bt�Bu�Bu�Bu�Bu�Bt�Bt�Bs�Bt�Bv�Bw�Bx�Bx�Bz�B{�B}�B� B�B�B�1B�JB�PB�VB�\B�{B��B��B��B��B�B�B�B�FB�^B�^B�XB�XB�XB�dBÖBȴB��B�B�B�B�)B�BB�`B�yB�yB�B��B��B��B��B��B��B	B	B	B	+B	
=B	\B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	&�B	)�B	,B	-B	/B	/B	2-B	49B	6FB	<jB	?}B	B�B	D�B	E�B	F�B	H�B	K�B	K�B	M�B	N�B	O�B	P�B	T�B	W
B	YB	[#B	\)B	]/B	`BB	cTB	e`B	gmB	iyB	jB	l�B	n�B	o�B	q�B	s�B	t�B	v�B	w�B	x�B	x�B	y�B	z�B	|�B	~�B	� B	�B	�+B	�7B	�DB	�JB	�\B	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�'B	�'B	�?B	�?B	�?B	�FB	�RB	�^B	�dB	�dB	�dB	�dB	�dB	�dB	�dB	�dB	�jB	�qB	��B	ÖB	ÖB	ƨB	ǮB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�)B	�)B	�5B	�5B	�;B	�BB	�NB	�TB	�fB	�mB	�mB	�mB	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B
	7B
1B
	7B
1B
+B
1B
	7B
	7B

=B
DB
DB
DB
JB
PB
JB
DB
JB
JB
PB
VB
\B
VB
\B
\B
\B
\B
\B
\B
bB
bB
oB
oB
oB
oB
uB
uB
oB
uB
oB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
 �B
 �B
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
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
-B
-B
-B
-B
-B
-B
-B
-B
.B
.B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
33B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
8RB
9XB
8RB
8RB
9XB
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
<jB
<jB
<jB
<jB
<jB
=qB
?}B
?}B
?}B
@�B
A�B
A�B
B�B
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
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
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
L�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
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
W
B
W
B
XB
XB
XB
XB
XB
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
YB
YB
YB
YB
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
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
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
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
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
gmB
gmB
gmB
gmB
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
hsB
hsB
iyB
iyB
iyB
iy11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B@OB@OB@OB?HB@OB@OB@OB@OB@OB@OB@OB?HB;0B]Bs�Bz�B|�B��B�<B�FB�MB�YB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�"B�"B�(B�4B�kB�qB��B�B��B��B��B�B�B�mB͟BѷBѷBбBΥB͟BʌB�zB�OB�*B��B��B�xB�MB��Bt�BkQBiDBd&BN�B*�B��B�KB�B��B@B�B��B�]B�>B��BɆB�aB�*B��B�SB{�BlWBc BP�B<6B+�B~B�B
�vB
��B
̘B
�OB
��B
��B
��B
�SB
��B
{�B
h>B
`B
\�B
V�B
I�B
6B
kB
�B	��B	��B	��B	��B	ɆB	�OB	��B	�?B	��B	��B	x�B	raB	nIB	bB	O�B	I�B	J�B	K�B	GzB	<B	2�B	'�B	~B	?B	.B	B	(B	(B	B	�B	�B	�B	�B��B��B�OB�B�B�B��BөB͟BɆB�gB�B��B��B�|B��B��B��B��B��B��B��B�EB�YB�eB�2B�B��B��B}�Bu�BpoBkQBl=BmCBlWBj0Bj0Bf2Bf2BgB`B]�B]�B\�BV�BR�BP�BN�BK�BI�BG_BCaB="B9$B6B4�B4�B1�B0�B.�B.�B-�B,�B,�B,�B*�B*�B)�B'�B'�B&�B&�B&�B%�B%�B$�B$�B#�B"�B"�B"�B!|B �B"�B!�B �B"�B#�B"�B"�B"�B"�B"�B&�B%�B&�B#�B"�B!|B#�B%�B#�B#�B#�B$�B'�B'�B&�B)�B)�B)�B/�B3�B3�B6�B6�B6�B8B8B8B8B;B:B;B<B=<B>(B<B?HBBABBABCGBFtBFYBESBFYBO�BR�BQ�BS�BW�BV�B]�B_�BbBe,Bi*Bi*Bi*Bj0BjKBnIBpUBqvBq[BraBtnBu�Bu�ButButBt�Bt�Bs�BtnBvzBw�Bx�Bx�Bz�B{�B}�B�B��B��B��B�B�B�"B�(B�FB�eB�pB��B��B��B��B��B��B�B�B�	B�	B�$B�B�GB�fBϑB��B��B��B��B��B�,B�*B�DB�WB�tB�zB��B��B��B��B	�B	�B	�B	�B		�B	B	2B	MB	SB	?B	KB	WB	~B	jB	!|B	"�B	&�B	)�B	+�B	,�B	.�B	.�B	1�B	4B	5�B	<B	?.B	BAB	DgB	ESB	FYB	HfB	K�B	K�B	M�B	N�B	O�B	P�B	T�B	V�B	X�B	Z�B	[�B	\�B	_�B	cB	e,B	g8B	i*B	j0B	l=B	nIB	oOB	qvB	shB	tnB	vzB	w�B	x�B	x�B	y�B	z�B	|�B	~�B	�B	��B	��B	��B	��B	��B	�B	�.B	�,B	�?B	�KB	�QB	�qB	�jB	�pB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�<B	�UB	�aB	�GB	�YB	�_B	�_B	�_B	�lB	�rB	�xB	̘B	�~B	̈́B	�~B	�~B	̈́B	̘B	̘B	ΥB	ϑB	ϑB	бB	бB	өB	յB	յB	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�8B	�B	�$B	�>B	�*B	�KB	�KB	�6B	�=B	�WB	�=B	�=B	�6B	�QB	�WB	�cB	�UB	�[B	�aB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
�B
�B
�B
�B
�B

	B

�B
B

�B
�B
B
�B
B
�B
B
B
B
(B
"B
(B
(B
B
(B
B
(B
B
B
 B
 B
 B
 B
&B
&B
 B
&B
 B
&B
@B
&B
&B
&B
&B
&B
&B
@B
&B
&B
@B
@B
@B
@B
&B
@B
,B
FB
,B
2B
2B
2B
9B
9B
?B
YB
?B
?B
YB
_B
EB
_B
KB
KB
QB
WB
WB
]B
dB
jB
jB
jB
jB
�B
pB
 vB
 vB
!|B
!�B
!�B
 vB
 vB
!�B
!�B
"�B
"�B
"�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
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
,�B
,�B
,�B
,�B
,�B
,�B
,�B
,�B
-�B
-�B
-�B
-�B
.�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
2�B
2�B
2�B
2�B
4�B
4�B
4�B
4�B
4�B
4�B
5B
6B
6�B
6�B
8B
9	B
8B
8B
9	B
9	B
9	B
:B
:B
:*B
;B
;B
;0B
;B
;0B
;B
<B
<B
<B
<B
<6B
<B
<6B
="B
?HB
?HB
?HB
@4B
AUB
A;B
B[B
BAB
BAB
BAB
BAB
CGB
CGB
CGB
DgB
DMB
ESB
EmB
ESB
ESB
ESB
FYB
FYB
FtB
G_B
G_B
G_B
G_B
G_B
GzB
GzB
G_B
G_B
H�B
IlB
I�B
IlB
IlB
J�B
J�B
JrB
KxB
K�B
KxB
K�B
L�B
L�B
L~B
L~B
L~B
L�B
L�B
L~B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
U�B
U�B
V�B
V�B
V�B
V�B
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
V�B
V�B
W�B
W�B
W�B
W�B
W�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
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
Z�B
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
_B
^�B
_�B
_�B
_�B
_�B
aB
`�B
`�B
`�B
aB
`�B
aB
a�B
a�B
bB
a�B
a�B
c B
cB
d&B
dB
dB
dB
d&B
e,B
fB
fB
fB
fB
f2B
f2B
fB
fB
f2B
fB
f2B
fB
g8B
g8B
g8B
gB
gB
g8B
gB
g8B
gB
gB
gB
gB
h>B
h$B
h>B
h$B
h>B
h$B
h$B
h>B
i*B
i*B
iDB
i*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.55(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807050043062018070500430620180705004306201807060042472018070600424720180706004247JA  ARFMdecpA19c                                                                20180723153656  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723070901  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723070903  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723070905  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723070905  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723070905  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723070906  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723070906  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180723070906  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180723070906                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723072025                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180630153957  CV  JULD            G�O�G�O�F�q�                JM  ARCAJMQC2.0                                                                 20180704154306  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180704154306  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180705154247  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                