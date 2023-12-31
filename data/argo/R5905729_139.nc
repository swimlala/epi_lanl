CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-02-15T10:01:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ߜ   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20220215100116  20220215100116  5905729 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @ٹ֔z�t1   @ٹ�'҈�@&�Z�1�d:�-1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B�33B�33B�  B�  B�  B�  B�33B���B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D���D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�33B�  B�33B�33B�  B�  B�  B�  B�33B���B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D���D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��#A��TA��HA��yA��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A��A��A��yA��yA��mA��TA��TA��TA��`A��HA��/A��#A��
A�A��A�A�S�A�XA�S�A�(�A�  A��A���A��A�S�A�ȴA�bNA��A��^A��-A�oA���A�~�A�n�A� �AA;dA}�TA{\)Ay�-Au�
Aq�#AhĜAa%A\VAW;dAP  AN  AIS�AH�\AG�hAF��AFjAEoAB��AAx�A@�!A?XA>5?A=+A<��A;�A;G�A;�A:��A:��A:�\A:v�A:M�A:�A9ƨA9p�A8��A8ZA7�wA6�+A5��A533A4JA2�RA1��A1l�A1K�A1+A0ȴA0�DA0jA0bNA0ZA0I�A/�TA-�A+�-A+?}A+VA*��A*��A*1'A)�TA)�A)�hA)G�A)
=A(��A(�DA(z�A(I�A'��A'��A'G�A&�A&��A&�uA&z�A&(�A%�wA%��A%dZA%VA$��A#�;A#&�A"��A"M�A" �A!��A!oA ��A ~�A 5?A�wAG�AȴA�uAffA5?A�A��A��A��AXA7LA�/A5?AAG�A�yA�RA^5A-A�A��Al�A�A�mA��A�^A��A��A|�A`BA&�AjA1A�FAp�A
=A�jAQ�A�A{A�TAS�A�A��A�DA^5A{AƨAG�A�HA��AI�A�FA?}A�`A1'A�AdZAG�A/A��AbA��A|�A;dA
�yA
��A
��A
n�A
Q�A
E�A
 �A	�^A	�A	�A	oA	A��A��Az�AƨAt�AG�A?}A;dA��AJA7LA�9A�AM�A�A�mAl�A�A�!A�+An�A5?AO�AA �/A ĜA �9A V@���@��P@�dZ@�33@��\@��@�@�Ĝ@��F@��@�o@�$�@�O�@���@�A�@�ƨ@�\)@��@���@�n�@�$�@��@��@�D@�r�@�A�@��m@�l�@�@�V@���@��`@�j@�K�@�v�@�@�j@�Q�@�  @���@���@���@�-@��@��;@�R@�~�@噚@��`@�@��;@�"�@��@◍@�-@�`B@�(�@ޟ�@ݲ-@�/@��;@�K�@ڏ\@�7L@؋D@�1'@��m@ם�@�dZ@֏\@�p�@�V@Ԭ@�r�@��@���@�dZ@�o@�v�@щ7@���@�9X@��m@Ͼw@ϥ�@ϕ�@υ@�|�@�dZ@�+@��y@�^5@͉7@̛�@�l�@�ȴ@ʧ�@�~�@��@Ɂ@�hs@�G�@��/@��@�K�@���@�=q@ź^@�&�@ă@���@�;d@§�@�E�@���@��#@��h@�`B@��@�1'@�ƨ@�\)@�o@��@��@�O�@�
=@��@���@�O�@��@��@���@���@��F@�"�@�~�@�@�@��7@��`@���@��D@�9X@�dZ@��\@�=q@�@��@��@�A�@���@�t�@�
=@�^5@�@��^@�X@�1'@� �@���@��@�V@���@�@���@�O�@�&�@�V@�Ĝ@�Z@���@�t�@��@���@�^5@�-@�J@���@��^@��h@�p�@�`B@�G�@�/@��@���@� �@��;@��F@���@�S�@��@�^5@�@��@���@��@���@���@�l�@�33@��+@�5?@�@��T@��#@��h@�X@�&�@�Ĝ@���@��@�t�@�;d@��y@�ȴ@�ff@��-@���@��9@��@�Z@�(�@��w@�K�@�~�@�-@��@�@�hs@�?}@�hs@�`B@��@���@��D@�bN@�9X@�C�@���@���@�v�@�$�@��#@��-@��@�X@��@���@��9@��u@�A�@��w@�
=@�E�@�-@�{@�p�@�7L@��@��/@��j@��u@�r�@�bN@�Q�@�9X@�(�@��;@�t�@�S�@�+@���@�^5@��@���@�7L@��@���@���@��u@�Z@���@���@���@�l�@�+@���@��!@�n�@�-@�J@���@��h@��@��@�?}@��@��j@���@��@�I�@�9X@��m@�dZ@�;d@��@���@�V@�5?@�$�@�{@��7@���@��9@��u@�9X@�w@|�@�@~�@~v�@~$�@}�T@}@}`B@|��@|�@|Z@{�
@{dZ@z�@z�!@z�\@zM�@y��@yX@x��@x1'@w\)@v�@v�+@v5?@v{@u@u�h@u/@tj@sƨ@s33@qX@p��@pĜ@p�u@p1'@o��@o�@o��@o��@ol�@n�@m�-@mp�@mO�@m/@l��@l��@l1@k@j~�@jn�@iX@h�@hQ�@h  @g�P@g;d@g+@g
=@f�@f��@fE�@e`B@d9X@c��@c�@ct�@cdZ@cdZ@cS�@cS�@cC�@c"�@b�H@b~�@b=q@a�@`A�@_\)@_K�@_�@^�y@^�y@^�y@^�y@^��@_
=@_+@_;d@_;d@_;d@_+@^��@^ff@]�T@]@]/@\(�@\�@[�
@[S�@Z^5@Y�@Yx�@X�@W�@Wl�@V�R@VV@V5?@V$�@V@U�@U�T@U@U�-@U?}@T�D@S��@S33@So@R~�@Q�7@Q7L@P��@P�9@P �@O��@Ol�@N��@N��@NE�@M�-@M?}@L��@LI�@Kƨ@KC�@J��@J^5@I��@I�@I�^@I�7@Ihs@IX@I%@H�9@HQ�@H  @F�@FV@E�h@EV@EV@EV@D��@D��@D��@DZ@C�
@C��@Ct�@B��@BJ@A��@AG�@@Ĝ@@�u@@bN@?�w@?��@?\)@?;d@?�@>�@>��@>E�@>@=�T@=@=@=�h@=`B@=O�@=?}@=/@<��@<�j@<z�@<I�@<�@<1@;�
@;S�@:�H@:=q@9��@8bN@8  @7�w@7�@7�P@7\)@7K�@7+@6��@6��@6ff@6{@5��@5p�@5?}@4��@4(�@3��@3�F@3�@3dZ@3C�@3"�@2��@2~�@2=q@2�@1�@1�7@1X@1&�@0�9@0�@0 �@/�w@/�@/�P@/l�@/l�@/\)@/K�@/;d@/
=@.v�@.E�@.{@-@-��@-p�@-?}@-/@-V@,�@,�/@,��@,�j@,��@,�D@,�D@+��@+33@*�!@*n�@*^5@*=q@*-@*�@)�#@)��@)hs@)X@)X@)X@)G�@)&�@(�u@(1'@'��@'\)@&��@&�y@&�@&ȴ@&�R@&�+@&@%��@%��@%p�@%`B@%O�@%/@%V@$�j@$z�@#��@#33@#o@#@"�H@"�\@"-@!�#@!�7@!hs@!G�@!�@ A�@�;@��@\)@��@E�@�-@p�@�@�/@��@��@��@@��@��@�\@~�@M�@�#@hs@%@�`@Ĝ@�9@�u@�u@r�@A�@b@�@��@�@��@�P@\)@�@�y@��@V@@@O�@��@��@�@9X@1@��@�m@�@C�@33@33@o@�@�H@�H@��@-@�@��@��@X@7L@%@Ĝ@�u@r�@bN@Q�@Q�@Q�@Q�@Q�@A�@1'@b@�@��@l�@�@��@�y@�@ȴ@��@��@v�@ff@V@5?@$�@{@��@�@O�@��@��@�@j@j@I�@I�@Z@Z@Z@��@��@t�@dZ111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��TA��#A��TA��HA��yA��A��A��A��A��A��A��A��A��A���A���A���A���A���A���A��A��A��yA��yA��mA��TA��TA��TA��`A��HA��/A��#A��
A�A��A�A�S�A�XA�S�A�(�A�  A��A���A��A�S�A�ȴA�bNA��A��^A��-A�oA���A�~�A�n�A� �AA;dA}�TA{\)Ay�-Au�
Aq�#AhĜAa%A\VAW;dAP  AN  AIS�AH�\AG�hAF��AFjAEoAB��AAx�A@�!A?XA>5?A=+A<��A;�A;G�A;�A:��A:��A:�\A:v�A:M�A:�A9ƨA9p�A8��A8ZA7�wA6�+A5��A533A4JA2�RA1��A1l�A1K�A1+A0ȴA0�DA0jA0bNA0ZA0I�A/�TA-�A+�-A+?}A+VA*��A*��A*1'A)�TA)�A)�hA)G�A)
=A(��A(�DA(z�A(I�A'��A'��A'G�A&�A&��A&�uA&z�A&(�A%�wA%��A%dZA%VA$��A#�;A#&�A"��A"M�A" �A!��A!oA ��A ~�A 5?A�wAG�AȴA�uAffA5?A�A��A��A��AXA7LA�/A5?AAG�A�yA�RA^5A-A�A��Al�A�A�mA��A�^A��A��A|�A`BA&�AjA1A�FAp�A
=A�jAQ�A�A{A�TAS�A�A��A�DA^5A{AƨAG�A�HA��AI�A�FA?}A�`A1'A�AdZAG�A/A��AbA��A|�A;dA
�yA
��A
��A
n�A
Q�A
E�A
 �A	�^A	�A	�A	oA	A��A��Az�AƨAt�AG�A?}A;dA��AJA7LA�9A�AM�A�A�mAl�A�A�!A�+An�A5?AO�AA �/A ĜA �9A V@���@��P@�dZ@�33@��\@��@�@�Ĝ@��F@��@�o@�$�@�O�@���@�A�@�ƨ@�\)@��@���@�n�@�$�@��@��@�D@�r�@�A�@��m@�l�@�@�V@���@��`@�j@�K�@�v�@�@�j@�Q�@�  @���@���@���@�-@��@��;@�R@�~�@噚@��`@�@��;@�"�@��@◍@�-@�`B@�(�@ޟ�@ݲ-@�/@��;@�K�@ڏ\@�7L@؋D@�1'@��m@ם�@�dZ@֏\@�p�@�V@Ԭ@�r�@��@���@�dZ@�o@�v�@щ7@���@�9X@��m@Ͼw@ϥ�@ϕ�@υ@�|�@�dZ@�+@��y@�^5@͉7@̛�@�l�@�ȴ@ʧ�@�~�@��@Ɂ@�hs@�G�@��/@��@�K�@���@�=q@ź^@�&�@ă@���@�;d@§�@�E�@���@��#@��h@�`B@��@�1'@�ƨ@�\)@�o@��@��@�O�@�
=@��@���@�O�@��@��@���@���@��F@�"�@�~�@�@�@��7@��`@���@��D@�9X@�dZ@��\@�=q@�@��@��@�A�@���@�t�@�
=@�^5@�@��^@�X@�1'@� �@���@��@�V@���@�@���@�O�@�&�@�V@�Ĝ@�Z@���@�t�@��@���@�^5@�-@�J@���@��^@��h@�p�@�`B@�G�@�/@��@���@� �@��;@��F@���@�S�@��@�^5@�@��@���@��@���@���@�l�@�33@��+@�5?@�@��T@��#@��h@�X@�&�@�Ĝ@���@��@�t�@�;d@��y@�ȴ@�ff@��-@���@��9@��@�Z@�(�@��w@�K�@�~�@�-@��@�@�hs@�?}@�hs@�`B@��@���@��D@�bN@�9X@�C�@���@���@�v�@�$�@��#@��-@��@�X@��@���@��9@��u@�A�@��w@�
=@�E�@�-@�{@�p�@�7L@��@��/@��j@��u@�r�@�bN@�Q�@�9X@�(�@��;@�t�@�S�@�+@���@�^5@��@���@�7L@��@���@���@��u@�Z@���@���@���@�l�@�+@���@��!@�n�@�-@�J@���@��h@��@��@�?}@��@��j@���@��@�I�@�9X@��m@�dZ@�;d@��@���@�V@�5?@�$�@�{@��7@���@��9@��u@�9X@�w@|�@�@~�@~v�@~$�@}�T@}@}`B@|��@|�@|Z@{�
@{dZ@z�@z�!@z�\@zM�@y��@yX@x��@x1'@w\)@v�@v�+@v5?@v{@u@u�h@u/@tj@sƨ@s33@qX@p��@pĜ@p�u@p1'@o��@o�@o��@o��@ol�@n�@m�-@mp�@mO�@m/@l��@l��@l1@k@j~�@jn�@iX@h�@hQ�@h  @g�P@g;d@g+@g
=@f�@f��@fE�@e`B@d9X@c��@c�@ct�@cdZ@cdZ@cS�@cS�@cC�@c"�@b�H@b~�@b=q@a�@`A�@_\)@_K�@_�@^�y@^�y@^�y@^�y@^��@_
=@_+@_;d@_;d@_;d@_+@^��@^ff@]�T@]@]/@\(�@\�@[�
@[S�@Z^5@Y�@Yx�@X�@W�@Wl�@V�R@VV@V5?@V$�@V@U�@U�T@U@U�-@U?}@T�D@S��@S33@So@R~�@Q�7@Q7L@P��@P�9@P �@O��@Ol�@N��@N��@NE�@M�-@M?}@L��@LI�@Kƨ@KC�@J��@J^5@I��@I�@I�^@I�7@Ihs@IX@I%@H�9@HQ�@H  @F�@FV@E�h@EV@EV@EV@D��@D��@D��@DZ@C�
@C��@Ct�@B��@BJ@A��@AG�@@Ĝ@@�u@@bN@?�w@?��@?\)@?;d@?�@>�@>��@>E�@>@=�T@=@=@=�h@=`B@=O�@=?}@=/@<��@<�j@<z�@<I�@<�@<1@;�
@;S�@:�H@:=q@9��@8bN@8  @7�w@7�@7�P@7\)@7K�@7+@6��@6��@6ff@6{@5��@5p�@5?}@4��@4(�@3��@3�F@3�@3dZ@3C�@3"�@2��@2~�@2=q@2�@1�@1�7@1X@1&�@0�9@0�@0 �@/�w@/�@/�P@/l�@/l�@/\)@/K�@/;d@/
=@.v�@.E�@.{@-@-��@-p�@-?}@-/@-V@,�@,�/@,��@,�j@,��@,�D@,�D@+��@+33@*�!@*n�@*^5@*=q@*-@*�@)�#@)��@)hs@)X@)X@)X@)G�@)&�@(�u@(1'@'��@'\)@&��@&�y@&�@&ȴ@&�R@&�+@&@%��@%��@%p�@%`B@%O�@%/@%V@$�j@$z�@#��@#33@#o@#@"�H@"�\@"-@!�#@!�7@!hs@!G�@!�@ A�@�;@��@\)@��@E�@�-@p�@�@�/@��@��@��@@��@��@�\@~�@M�@�#@hs@%@�`@Ĝ@�9@�u@�u@r�@A�@b@�@��@�@��@�P@\)@�@�y@��@V@@@O�@��@��@�@9X@1@��@�m@�@C�@33@33@o@�@�H@�H@��@-@�@��@��@X@7L@%@Ĝ@�u@r�@bN@Q�@Q�@Q�@Q�@Q�@A�@1'@b@�@��@l�@�@��@�y@�@ȴ@��@��@v�@ff@V@5?@$�@{@��@�@O�@��@��@�@j@j@I�@I�@Z@Z@Z@��@��@t�@dZ111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�#B	�#B	�#B	�B	�B	�)B	�HB	�TB	�TB	�BB	�NB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B
�B	��B
T�B
��BVBaHBM�BR�BG�B6FB
ȴB
9XB	�;B
R�B
6FB	��B	�dB	�mB
1B
B	�B	��B	��B	�=B	bNB�B�'B�TB�jB��B	B�B	J�B	R�B	^5B	`BB	XB	XB	z�B	��B	��B	�3B	��B	�B
1B
+B
Q�B
^5B
k�B
n�B
o�B
p�B
s�B
u�B
{�B
~�B
�%B
�=B
�1B
�hB
��B
��B
��B
��B
�qB
B
B
��B
ŢB
ȴB
��B
ɺB
ÖB
�RB
��B
��B
ĜB
��B
��B
ɺB
ÖB
ȴB
��B
��B
ȴB
ɺB
ɺB
��B
��B
ȴB
ŢB
ÖB
ƨB
B
ƨB
ŢB
ŢB
��B
�wB
B
�wB
�^B
�?B
�3B
�!B
�RB
�LB
�XB
�?B
�B
�3B
�LB
�?B
�B
�!B
�B
�FB
�?B
�?B
�?B
�9B
�'B
�!B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
�JB
�VB
�VB
�JB
�7B
�7B
�+B
�=B
�PB
�1B
�B
�B
�B
�B
�B
� B
|�B
w�B
y�B
x�B
v�B
q�B
p�B
r�B
l�B
k�B
u�B
t�B
q�B
jB
iyB
l�B
p�B
n�B
n�B
o�B
q�B
n�B
o�B
o�B
m�B
iyB
jB
hsB
l�B
k�B
hsB
gmB
e`B
]/B
aHB
cTB
dZB
bNB
\)B
O�B
Q�B
R�B
YB
YB
W
B
T�B
O�B
N�B
Q�B
S�B
R�B
M�B
D�B
L�B
Q�B
Q�B
O�B
J�B
I�B
Q�B
O�B
N�B
J�B
J�B
J�B
E�B
C�B
J�B
G�B
B�B
C�B
C�B
F�B
E�B
D�B
D�B
E�B
E�B
D�B
C�B
A�B
<jB
D�B
C�B
@�B
=qB
=qB
9XB
:^B
33B
6FB
2-B
2-B
6FB
1'B
7LB
7LB
:^B
6FB
.B
,B
)�B
%�B
&�B
.B
'�B
(�B
+B
'�B
&�B
&�B
)�B
%�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
!�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
"�B
!�B
 �B
�B
�B
�B
uB
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
{B
%B
B
oB
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
!�B
!�B
!�B
"�B
!�B
 �B
�B
�B
�B
 �B
"�B
"�B
�B
�B
�B
�B
�B
#�B
,B
+B
)�B
+B
(�B
%�B
)�B
-B
-B
-B
,B
+B
+B
'�B
&�B
)�B
/B
.B
,B
-B
)�B
'�B
)�B
1'B
33B
33B
1'B
.B
-B
,B
/B
2-B
49B
49B
6FB
9XB
8RB
49B
33B
33B
33B
1'B
+B
0!B
49B
5?B
49B
5?B
6FB
6FB
6FB
49B
5?B
33B
33B
1'B
.B
-B
.B
6FB
6FB
1'B
7LB
9XB
8RB
9XB
9XB
:^B
;dB
;dB
:^B
9XB
7LB
6FB
:^B
:^B
7LB
7LB
8RB
:^B
;dB
>wB
=qB
=qB
?}B
=qB
<jB
?}B
?}B
>wB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
A�B
D�B
C�B
B�B
A�B
C�B
D�B
C�B
C�B
C�B
A�B
?}B
C�B
B�B
C�B
D�B
F�B
F�B
E�B
A�B
A�B
E�B
G�B
F�B
F�B
J�B
J�B
K�B
J�B
K�B
L�B
L�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
M�B
N�B
M�B
K�B
L�B
J�B
K�B
K�B
L�B
N�B
N�B
O�B
N�B
N�B
L�B
J�B
K�B
K�B
F�B
O�B
Q�B
Q�B
P�B
Q�B
R�B
S�B
S�B
P�B
N�B
L�B
S�B
T�B
S�B
R�B
O�B
N�B
M�B
O�B
R�B
N�B
O�B
T�B
S�B
S�B
T�B
W
B
VB
T�B
S�B
R�B
O�B
O�B
VB
ZB
[#B
[#B
[#B
ZB
ZB
YB
XB
W
B
T�B
T�B
P�B
R�B
S�B
ZB
ZB
[#B
]/B
]/B
]/B
_;B
_;B
_;B
_;B
_;B
^5B
]/B
\)B
[#B
[#B
]/B
[#B
XB
^5B
\)B
[#B
YB
\)B
\)B
ZB
]/B
^5B
_;B
aHB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
`BB
_;B
_;B
cTB
dZB
bNB
`BB
ffB
ffB
ffB
e`B
ffB
gmB
ffB
gmB
gmB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
jB
jB
l�B
l�B
l�B
l�B
l�B
k�B
jB
jB
jB
gmB
jB
jB
m�B
o�B
o�B
o�B
n�B
m�B
m�B
l�B
n�B
n�B
l�B
m�B
p�B
n�B
p�B
q�B
q�B
p�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
t�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
r�B
q�B
q�B
p�B
p�B
t�B
v�B
w�B
w�B
w�B
w�B
w�B
v�B
v�B
w�B
w�B
v�B
w�B
w�B
w�B
v�B
y�B
x�B
x�B
y�B
y�B
x�B
x�B
x�B
y�B
y�B
y�B
x�B
y�B
y�B
x�B
y�B
y�B
y�B
|�B
|�B
|�B
}�B
}�B
|�B
|�B
|�B
{�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�+B
�%B
�B
�+B
�+B
�+B
�1B
�1B
�1B
�+B
�%B
�%B
�%B
�%B
�7B
�7B
�7B
�1B
�1B
�7B
�7B
�=B
�7B
�1B
�+B
�1B
�=B
�=B
�=B
�1B
�1B
�7B
�7B
�=B
�=B
�=B
�1B
�=B
�JB
�JB
�PB
�JB
�JB
�DB
�DB
�JB
�PB
�VB
�VB
�VB
�\B
�VB
�VB
�VB
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�\B
�bB
�bB
�bB
�bB
�hB
�oB
�oB
�hB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�uB
�oB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�
B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�#B	�#B	�#B	�B	�B	�)B	�HB	�TB	�TB	�BB	�NB	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B
�B	��B
T�B
��BVBaHBM�BR�BG�B6FB
ȴB
9XB	�;B
R�B
6FB	��B	�dB	�mB
1B
B	�B	��B	��B	�=B	bNB�B�'B�TB�jB��B	B�B	J�B	R�B	^5B	`BB	XB	XB	z�B	��B	��B	�3B	��B	�B
1B
+B
Q�B
^5B
k�B
n�B
o�B
p�B
s�B
u�B
{�B
~�B
�%B
�=B
�1B
�hB
��B
��B
��B
��B
�qB
B
B
��B
ŢB
ȴB
��B
ɺB
ÖB
�RB
��B
��B
ĜB
��B
��B
ɺB
ÖB
ȴB
��B
��B
ȴB
ɺB
ɺB
��B
��B
ȴB
ŢB
ÖB
ƨB
B
ƨB
ŢB
ŢB
��B
�wB
B
�wB
�^B
�?B
�3B
�!B
�RB
�LB
�XB
�?B
�B
�3B
�LB
�?B
�B
�!B
�B
�FB
�?B
�?B
�?B
�9B
�'B
�!B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
�JB
�VB
�VB
�JB
�7B
�7B
�+B
�=B
�PB
�1B
�B
�B
�B
�B
�B
� B
|�B
w�B
y�B
x�B
v�B
q�B
p�B
r�B
l�B
k�B
u�B
t�B
q�B
jB
iyB
l�B
p�B
n�B
n�B
o�B
q�B
n�B
o�B
o�B
m�B
iyB
jB
hsB
l�B
k�B
hsB
gmB
e`B
]/B
aHB
cTB
dZB
bNB
\)B
O�B
Q�B
R�B
YB
YB
W
B
T�B
O�B
N�B
Q�B
S�B
R�B
M�B
D�B
L�B
Q�B
Q�B
O�B
J�B
I�B
Q�B
O�B
N�B
J�B
J�B
J�B
E�B
C�B
J�B
G�B
B�B
C�B
C�B
F�B
E�B
D�B
D�B
E�B
E�B
D�B
C�B
A�B
<jB
D�B
C�B
@�B
=qB
=qB
9XB
:^B
33B
6FB
2-B
2-B
6FB
1'B
7LB
7LB
:^B
6FB
.B
,B
)�B
%�B
&�B
.B
'�B
(�B
+B
'�B
&�B
&�B
)�B
%�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
"�B
!�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
"�B
"�B
!�B
 �B
�B
�B
�B
uB
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
{B
%B
B
oB
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
!�B
!�B
!�B
"�B
!�B
 �B
�B
�B
�B
 �B
"�B
"�B
�B
�B
�B
�B
�B
#�B
,B
+B
)�B
+B
(�B
%�B
)�B
-B
-B
-B
,B
+B
+B
'�B
&�B
)�B
/B
.B
,B
-B
)�B
'�B
)�B
1'B
33B
33B
1'B
.B
-B
,B
/B
2-B
49B
49B
6FB
9XB
8RB
49B
33B
33B
33B
1'B
+B
0!B
49B
5?B
49B
5?B
6FB
6FB
6FB
49B
5?B
33B
33B
1'B
.B
-B
.B
6FB
6FB
1'B
7LB
9XB
8RB
9XB
9XB
:^B
;dB
;dB
:^B
9XB
7LB
6FB
:^B
:^B
7LB
7LB
8RB
:^B
;dB
>wB
=qB
=qB
?}B
=qB
<jB
?}B
?}B
>wB
>wB
?}B
?}B
@�B
@�B
A�B
A�B
A�B
D�B
C�B
B�B
A�B
C�B
D�B
C�B
C�B
C�B
A�B
?}B
C�B
B�B
C�B
D�B
F�B
F�B
E�B
A�B
A�B
E�B
G�B
F�B
F�B
J�B
J�B
K�B
J�B
K�B
L�B
L�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
M�B
N�B
M�B
K�B
L�B
J�B
K�B
K�B
L�B
N�B
N�B
O�B
N�B
N�B
L�B
J�B
K�B
K�B
F�B
O�B
Q�B
Q�B
P�B
Q�B
R�B
S�B
S�B
P�B
N�B
L�B
S�B
T�B
S�B
R�B
O�B
N�B
M�B
O�B
R�B
N�B
O�B
T�B
S�B
S�B
T�B
W
B
VB
T�B
S�B
R�B
O�B
O�B
VB
ZB
[#B
[#B
[#B
ZB
ZB
YB
XB
W
B
T�B
T�B
P�B
R�B
S�B
ZB
ZB
[#B
]/B
]/B
]/B
_;B
_;B
_;B
_;B
_;B
^5B
]/B
\)B
[#B
[#B
]/B
[#B
XB
^5B
\)B
[#B
YB
\)B
\)B
ZB
]/B
^5B
_;B
aHB
dZB
dZB
dZB
dZB
dZB
cTB
cTB
`BB
_;B
_;B
cTB
dZB
bNB
`BB
ffB
ffB
ffB
e`B
ffB
gmB
ffB
gmB
gmB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
jB
jB
l�B
l�B
l�B
l�B
l�B
k�B
jB
jB
jB
gmB
jB
jB
m�B
o�B
o�B
o�B
n�B
m�B
m�B
l�B
n�B
n�B
l�B
m�B
p�B
n�B
p�B
q�B
q�B
p�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
t�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
t�B
s�B
r�B
q�B
q�B
p�B
p�B
t�B
v�B
w�B
w�B
w�B
w�B
w�B
v�B
v�B
w�B
w�B
v�B
w�B
w�B
w�B
v�B
y�B
x�B
x�B
y�B
y�B
x�B
x�B
x�B
y�B
y�B
y�B
x�B
y�B
y�B
x�B
y�B
y�B
y�B
|�B
|�B
|�B
}�B
}�B
|�B
|�B
|�B
{�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�+B
�%B
�B
�+B
�+B
�+B
�1B
�1B
�1B
�+B
�%B
�%B
�%B
�%B
�7B
�7B
�7B
�1B
�1B
�7B
�7B
�=B
�7B
�1B
�+B
�1B
�=B
�=B
�=B
�1B
�1B
�7B
�7B
�=B
�=B
�=B
�1B
�=B
�JB
�JB
�PB
�JB
�JB
�DB
�DB
�JB
�PB
�VB
�VB
�VB
�\B
�VB
�VB
�VB
�\B
�\B
�\B
�bB
�bB
�bB
�bB
�bB
�\B
�bB
�bB
�bB
�bB
�hB
�oB
�oB
�hB
�oB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�uB
�oB
�{B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220215100116                              AO  ARCAADJP                                                                    20220215100116    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220215100116  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220215100116  QCF$                G�O�G�O�G�O�4000            