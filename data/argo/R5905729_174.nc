CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-01-31T08:01:26Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20230131080126  20230131080126  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�R���s1   @�S'҆�@*5\(��d������1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @���@�  @���A   A@  A`  A�  A�  A�  A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�  @���A   A@  A`  A�  A�  A�  A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AҶFAҶFAҾwA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���AҸRA�{AсAв-A�+A�E�A�z�A�v�A�;dAϥ�A��A���A�\)A͗�A�A�E�A�|�AǙ�A���A�33A��FA��/A�n�A��yA��A�  A�ƨA�E�A�v�A�1A��A�ȴA��A�^5A�"�A�r�A�\)A�1'A���A��A�ffA�(�A���A�ZA�JA��7A��-A�XA��FA���A��FA��FA��!A���A�|�A��A�M�A� �Au?}Ar��Ap1Aj�Af1A^��AZ�DAY33AW�AP��AN$�AJr�AG�wAF1AD�RA@��A>^5A<{A9��A81A7�PA7�7A7dZA6��A6�+A61A5&�A4��A4�DA4jA4r�A4z�A4�A3��A1�mA1l�A0�A.�`A,-A)�-A&5?A&��A%ƨA#��A!�wA ~�A�A��A�AdZAVAVA�A%A��A�A(�A�A��AĜA��A~�AffAJA�AA��Ax�A�AĜA�uA�+AbNA1'A"�A�RA��A�A-A��A�HAjA��A7LAVA�A�AAC�A�9A=qA(�A9XAJA�-A�A%A�PA�PA��A�A�
A�^A�wAp�A
ȴA
I�A	�-A�A��A��A1'AdZA��A  AVAz�A~�A�mA��A�;A��AhsAK�AoA�jA�!AM�AbA1A�;Al�AoA I�@���@�|�@��H@��+@��#@�/@���@���@���@���@��@��#@���@��@�7L@��9@���@�"�@�V@��-@�r�@�w@�$�@�&�@��@���@�r�@���@�\@�@�  @�5?@��`@�(�@���@�P@�K�@��@�\)@�F@���@���@�I�@��@߮@ߍP@���@�V@�J@ݺ^@݁@�7L@ܴ9@�1@�;d@�~�@ٺ^@�?}@���@�Z@ץ�@��@֏\@�=q@��#@���@�9X@�C�@���@ҸR@җ�@�5?@���@�A�@ϥ�@Ώ\@�=q@���@��T@ͺ^@�hs@̓u@��
@�\)@�33@�~�@ɲ-@���@ț�@ȃ@�Z@�1'@���@�l�@�C�@�"�@��y@���@Ų-@ģ�@�A�@�"�@�M�@��@��^@�O�@��@���@�bN@�ƨ@�+@��R@�E�@�{@��@��-@�G�@��@���@�Z@�1'@� �@���@���@�n�@�v�@��+@�V@�@��h@���@�Z@�|�@�ff@��@��T@�@�x�@�G�@��`@�z�@� �@�ƨ@�K�@�dZ@��!@��-@�O�@�O�@�?}@��`@���@���@�  @�ƨ@���@��;@��
@���@��@���@���@���@��@��@��F@��@�S�@��@��@��@��#@��-@��h@�/@�r�@���@�|�@�;d@���@�V@�{@��@��^@��h@�&�@��@�r�@�I�@�b@��;@���@�;d@��@���@�V@�{@���@�hs@�%@���@�1@��@���@���@��/@��j@��u@�z�@�bN@� �@��m@�dZ@��R@�ff@�E�@�{@��#@��h@�V@�%@���@��/@��9@�Z@�1'@�b@��;@���@��w@�t�@�"�@��\@�5?@�$�@�J@�@���@��@��#@�p�@��/@��D@�bN@�9X@��@�1@��@���@�dZ@���@�M�@���@�X@�7L@��`@���@��@�9X@��@���@�33@�
=@���@�5?@�J@���@���@��^@�X@��@��@��/@�Ĝ@��@��
@���@��@�|�@��@�ȴ@��!@��\@��#@�X@�&�@���@�r�@�9X@� �@�1@��@���@���@���@�v�@�^5@�5?@��@���@�G�@�V@��`@��D@�1'@��@��@��P@�S�@��@�o@���@�ȴ@��!@���@���@���@�ff@�J@��^@�p�@�X@�7L@�V@��/@��@��D@�bN@�1@�K�@��y@���@�ȴ@���@�M�@�{@�@��T@��#@���@�7L@��9@�9X@��@|�@~�@~V@~@}�T@}��@}�@|�@|��@|z�@|Z@|I�@{��@{dZ@z�@z~�@y�7@x��@w�P@w
=@v�+@v$�@u@u`B@t�/@tz�@s�
@sdZ@r�@rJ@qhs@p�9@p  @oK�@nff@m��@m?}@l�j@l��@lj@l�@k�
@k�@k�@k�@kdZ@j~�@jJ@i�#@i��@i%@h�u@h �@h  @h  @g�@g�P@g
=@f�y@f�@fV@e��@e�-@e��@ep�@d�j@d9X@d�@c�F@ct�@b��@b=q@a�^@a�@`Q�@`  @_��@_��@_|�@_\)@^��@^��@^E�@^@]@]`B@\�/@\z�@\9X@\�@\1@[�
@[t�@[C�@Z��@Y�#@XĜ@W�@W��@W��@W��@W�P@W�P@W\)@V$�@Up�@T�j@T1@S�@So@R��@R~�@R-@Q��@Q�^@Qhs@Qhs@QX@QG�@Q�@Q�@P��@P �@OK�@N�y@N@MO�@MV@L�@L�/@L�D@K�
@K��@KdZ@J�@J��@J�!@J��@JJ@I�7@IX@H�9@H �@H  @G��@G�@G|�@F�+@E��@E�@E/@Dz�@D9X@CdZ@B��@B�\@B~�@Bn�@B^5@B^5@B^5@A�#@@��@@A�@@b@@  @?��@?l�@?\)@?+@>�@>v�@>V@>E�@>E�@>{@=�h@=O�@=V@<�@<z�@<j@<I�@;�m@;�F@;S�@;@:��@:�!@:��@:n�@:-@9�@9��@9G�@9%@8r�@81'@7�@7l�@7K�@6�y@6��@6ff@6V@6V@65?@5�@5��@5p�@5O�@5V@4�j@4j@4�@3��@3�
@3��@3��@3��@3�@3C�@2�@2��@1�@1%@0�`@0�9@0�@0�@0bN@/��@/��@/��@/K�@.ff@-�T@-@-��@-p�@-?}@,��@,Z@+�m@+��@+�@+t�@+33@*��@*^5@*=q@*�@)�#@)�^@)��@)��@)��@)��@)�7@)�7@)x�@)G�@(��@(��@(Ĝ@(�9@(�u@(bN@( �@( �@(b@'�@'��@'�@'�@'�P@'|�@'\)@'+@&�y@&�@&��@&��@&�+@&v�@&V@&E�@&$�@%`B@$��@$��@$z�@$9X@#��@#ƨ@#��@#@"M�@"-@"J@!��@!��@!&�@ Ĝ@ �@ bN@  �@�P@�y@ȴ@�R@��@��@�+@�+@ff@V@V@V@V@V@E�@5?@�T@�/@�m@t�@�@��@~�@n�@^5@^5@J@��@�`@��@1'@  @��@�w@�P@K�@
=@ȴ@��@��@V@@��@�h@?}@�/@j@Z@(�@�
@33@�\@=q@J@�^@7L@&�@�@�@��@��@ �@�;@�@|�@l�@K�@�R@v�@ff@V@V@E�@5?@$�@$�@$�@{@{@@�@��@�-@�-@��@p�@`B@?}@/@�@�@�j@��@z�@Z@I�@��@�
@�F@��@��@�@t�@dZ@S�@C�@33@o@@
�@
�@
��@
�\@
~�@	��@	�@	�@	�#@	�^@	�^@	��@	�7@	hs@	hs@	G�@	7L@	7L@	7L@	&�@	�@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111AҶFAҶFAҾwA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���AҸRA�{AсAв-A�+A�E�A�z�A�v�A�;dAϥ�A��A���A�\)A͗�A�A�E�A�|�AǙ�A���A�33A��FA��/A�n�A��yA��A�  A�ƨA�E�A�v�A�1A��A�ȴA��A�^5A�"�A�r�A�\)A�1'A���A��A�ffA�(�A���A�ZA�JA��7A��-A�XA��FA���A��FA��FA��!A���A�|�A��A�M�A� �Au?}Ar��Ap1Aj�Af1A^��AZ�DAY33AW�AP��AN$�AJr�AG�wAF1AD�RA@��A>^5A<{A9��A81A7�PA7�7A7dZA6��A6�+A61A5&�A4��A4�DA4jA4r�A4z�A4�A3��A1�mA1l�A0�A.�`A,-A)�-A&5?A&��A%ƨA#��A!�wA ~�A�A��A�AdZAVAVA�A%A��A�A(�A�A��AĜA��A~�AffAJA�AA��Ax�A�AĜA�uA�+AbNA1'A"�A�RA��A�A-A��A�HAjA��A7LAVA�A�AAC�A�9A=qA(�A9XAJA�-A�A%A�PA�PA��A�A�
A�^A�wAp�A
ȴA
I�A	�-A�A��A��A1'AdZA��A  AVAz�A~�A�mA��A�;A��AhsAK�AoA�jA�!AM�AbA1A�;Al�AoA I�@���@�|�@��H@��+@��#@�/@���@���@���@���@��@��#@���@��@�7L@��9@���@�"�@�V@��-@�r�@�w@�$�@�&�@��@���@�r�@���@�\@�@�  @�5?@��`@�(�@���@�P@�K�@��@�\)@�F@���@���@�I�@��@߮@ߍP@���@�V@�J@ݺ^@݁@�7L@ܴ9@�1@�;d@�~�@ٺ^@�?}@���@�Z@ץ�@��@֏\@�=q@��#@���@�9X@�C�@���@ҸR@җ�@�5?@���@�A�@ϥ�@Ώ\@�=q@���@��T@ͺ^@�hs@̓u@��
@�\)@�33@�~�@ɲ-@���@ț�@ȃ@�Z@�1'@���@�l�@�C�@�"�@��y@���@Ų-@ģ�@�A�@�"�@�M�@��@��^@�O�@��@���@�bN@�ƨ@�+@��R@�E�@�{@��@��-@�G�@��@���@�Z@�1'@� �@���@���@�n�@�v�@��+@�V@�@��h@���@�Z@�|�@�ff@��@��T@�@�x�@�G�@��`@�z�@� �@�ƨ@�K�@�dZ@��!@��-@�O�@�O�@�?}@��`@���@���@�  @�ƨ@���@��;@��
@���@��@���@���@���@��@��@��F@��@�S�@��@��@��@��#@��-@��h@�/@�r�@���@�|�@�;d@���@�V@�{@��@��^@��h@�&�@��@�r�@�I�@�b@��;@���@�;d@��@���@�V@�{@���@�hs@�%@���@�1@��@���@���@��/@��j@��u@�z�@�bN@� �@��m@�dZ@��R@�ff@�E�@�{@��#@��h@�V@�%@���@��/@��9@�Z@�1'@�b@��;@���@��w@�t�@�"�@��\@�5?@�$�@�J@�@���@��@��#@�p�@��/@��D@�bN@�9X@��@�1@��@���@�dZ@���@�M�@���@�X@�7L@��`@���@��@�9X@��@���@�33@�
=@���@�5?@�J@���@���@��^@�X@��@��@��/@�Ĝ@��@��
@���@��@�|�@��@�ȴ@��!@��\@��#@�X@�&�@���@�r�@�9X@� �@�1@��@���@���@���@�v�@�^5@�5?@��@���@�G�@�V@��`@��D@�1'@��@��@��P@�S�@��@�o@���@�ȴ@��!@���@���@���@�ff@�J@��^@�p�@�X@�7L@�V@��/@��@��D@�bN@�1@�K�@��y@���@�ȴ@���@�M�@�{@�@��T@��#@���@�7L@��9@�9X@��@|�@~�@~V@~@}�T@}��@}�@|�@|��@|z�@|Z@|I�@{��@{dZ@z�@z~�@y�7@x��@w�P@w
=@v�+@v$�@u@u`B@t�/@tz�@s�
@sdZ@r�@rJ@qhs@p�9@p  @oK�@nff@m��@m?}@l�j@l��@lj@l�@k�
@k�@k�@k�@kdZ@j~�@jJ@i�#@i��@i%@h�u@h �@h  @h  @g�@g�P@g
=@f�y@f�@fV@e��@e�-@e��@ep�@d�j@d9X@d�@c�F@ct�@b��@b=q@a�^@a�@`Q�@`  @_��@_��@_|�@_\)@^��@^��@^E�@^@]@]`B@\�/@\z�@\9X@\�@\1@[�
@[t�@[C�@Z��@Y�#@XĜ@W�@W��@W��@W��@W�P@W�P@W\)@V$�@Up�@T�j@T1@S�@So@R��@R~�@R-@Q��@Q�^@Qhs@Qhs@QX@QG�@Q�@Q�@P��@P �@OK�@N�y@N@MO�@MV@L�@L�/@L�D@K�
@K��@KdZ@J�@J��@J�!@J��@JJ@I�7@IX@H�9@H �@H  @G��@G�@G|�@F�+@E��@E�@E/@Dz�@D9X@CdZ@B��@B�\@B~�@Bn�@B^5@B^5@B^5@A�#@@��@@A�@@b@@  @?��@?l�@?\)@?+@>�@>v�@>V@>E�@>E�@>{@=�h@=O�@=V@<�@<z�@<j@<I�@;�m@;�F@;S�@;@:��@:�!@:��@:n�@:-@9�@9��@9G�@9%@8r�@81'@7�@7l�@7K�@6�y@6��@6ff@6V@6V@65?@5�@5��@5p�@5O�@5V@4�j@4j@4�@3��@3�
@3��@3��@3��@3�@3C�@2�@2��@1�@1%@0�`@0�9@0�@0�@0bN@/��@/��@/��@/K�@.ff@-�T@-@-��@-p�@-?}@,��@,Z@+�m@+��@+�@+t�@+33@*��@*^5@*=q@*�@)�#@)�^@)��@)��@)��@)��@)�7@)�7@)x�@)G�@(��@(��@(Ĝ@(�9@(�u@(bN@( �@( �@(b@'�@'��@'�@'�@'�P@'|�@'\)@'+@&�y@&�@&��@&��@&�+@&v�@&V@&E�@&$�@%`B@$��@$��@$z�@$9X@#��@#ƨ@#��@#@"M�@"-@"J@!��@!��@!&�@ Ĝ@ �@ bN@  �@�P@�y@ȴ@�R@��@��@�+@�+@ff@V@V@V@V@V@E�@5?@�T@�/@�m@t�@�@��@~�@n�@^5@^5@J@��@�`@��@1'@  @��@�w@�P@K�@
=@ȴ@��@��@V@@��@�h@?}@�/@j@Z@(�@�
@33@�\@=q@J@�^@7L@&�@�@�@��@��@ �@�;@�@|�@l�@K�@�R@v�@ff@V@V@E�@5?@$�@$�@$�@{@{@@�@��@�-@�-@��@p�@`B@?}@/@�@�@�j@��@z�@Z@I�@��@�
@�F@��@��@�@t�@dZ@S�@C�@33@o@@
�@
�@
��@
�\@
~�@	��@	�@	�@	�#@	�^@	�^@	��@	�7@	hs@	hs@	G�@	7L@	7L@	7L@	&�@	�@	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	}�B	z�B	��B	�!B	�;B
%B
!�B
u�B
�sBG�B�{B��B�!B�9B�B�wB�dB�}B�jB�BK�BP�BVBW
BJ�B=qB-B+B?}B5?B6FB,B(�B�B#�BPB�B�
B��Bv�Bv�BbNBR�B9XB(�B�B
��B
�B
�HB
��B
�VB
�JB
u�B
hsB
XB
7LB
)�B
�B	��B	B	�B	z�B	YB	/B	/B	0!B	�B��B	B��B��B	DB	B��B	DB	�B	"�B	49B	O�B	XB	W
B	W
B	XB	VB	R�B	ZB	_;B	`BB	dZB	cTB	aHB	[#B	Q�B	`BB	YB	I�B	T�B	iyB	l�B	�oB	��B	�!B	�B	�FB	ŢB	�
B	�B	�B	��B	��B	��B
B
B

=B
+B
B
bB
�B
 �B
"�B
"�B
 �B
#�B
$�B
&�B
'�B
%�B
$�B
%�B
&�B
$�B
!�B
�B
 �B
%�B
$�B
!�B
�B
�B
�B
 �B
�B
"�B
#�B
�B
�B
 �B
 �B
#�B
(�B
)�B
'�B
$�B
�B
�B
DB	��B
  B
%B
�B
�B
&�B
%�B
 �B
�B
�B
�B
�B
!�B
�B
�B
�B
�B
'�B
,B
(�B
!�B
�B
�B
�B
!�B
"�B
 �B
�B
!�B
�B
 �B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
bB
�B
�B
�B
�B
{B
oB
\B
VB
JB

=B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B
B
+B
B	��B
B
+B
%B
%B
B
B
1B
+B
+B
%B
B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B	��B	��B
  B	��B
B
B
B
B
B	��B
B
B
B	��B	��B	��B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B
  B
B
%B
B
%B
B
B
B
%B
1B
1B
DB
JB
DB

=B
JB
JB
DB
PB
VB
VB
DB
bB
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
"�B
#�B
#�B
 �B
#�B
$�B
$�B
"�B
 �B
#�B
#�B
$�B
$�B
%�B
&�B
'�B
&�B
&�B
%�B
%�B
'�B
'�B
'�B
'�B
'�B
&�B
'�B
%�B
%�B
$�B
#�B
$�B
$�B
#�B
"�B
!�B
�B
 �B
#�B
)�B
)�B
+B
+B
)�B
)�B
'�B
'�B
,B
-B
-B
-B
-B
,B
0!B
0!B
/B
/B
.B
0!B
0!B
0!B
0!B
0!B
.B
.B
.B
0!B
2-B
2-B
33B
2-B
2-B
1'B
/B
/B
1'B
2-B
33B
33B
33B
33B
1'B
/B
.B
2-B
1'B
49B
5?B
49B
6FB
49B
49B
49B
5?B
49B
6FB
5?B
6FB
8RB
9XB
8RB
8RB
7LB
8RB
8RB
9XB
8RB
7LB
5?B
8RB
;dB
:^B
9XB
:^B
;dB
:^B
8RB
:^B
<jB
<jB
<jB
=qB
>wB
>wB
=qB
;dB
;dB
=qB
?}B
?}B
>wB
>wB
=qB
<jB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
D�B
C�B
C�B
C�B
D�B
D�B
D�B
B�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
B�B
A�B
D�B
G�B
G�B
G�B
F�B
F�B
H�B
G�B
G�B
F�B
D�B
E�B
F�B
F�B
G�B
F�B
F�B
H�B
I�B
H�B
I�B
H�B
H�B
I�B
I�B
I�B
H�B
G�B
H�B
H�B
G�B
H�B
G�B
K�B
L�B
L�B
M�B
N�B
N�B
O�B
N�B
O�B
O�B
N�B
N�B
N�B
O�B
P�B
P�B
R�B
S�B
T�B
W
B
W
B
W
B
W
B
XB
YB
XB
W
B
VB
W
B
XB
XB
W
B
XB
YB
ZB
ZB
ZB
YB
YB
ZB
ZB
YB
YB
[#B
[#B
ZB
YB
YB
[#B
ZB
ZB
YB
YB
ZB
ZB
ZB
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
]/B
]/B
]/B
\)B
]/B
^5B
_;B
_;B
^5B
]/B
]/B
\)B
[#B
[#B
\)B
aHB
bNB
aHB
aHB
`BB
_;B
\)B
]/B
_;B
_;B
aHB
aHB
bNB
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
bNB
aHB
`BB
bNB
aHB
cTB
ffB
ffB
ffB
e`B
dZB
gmB
gmB
ffB
gmB
gmB
gmB
e`B
e`B
gmB
ffB
ffB
iyB
iyB
iyB
hsB
ffB
gmB
iyB
iyB
hsB
iyB
hsB
iyB
l�B
l�B
l�B
l�B
l�B
k�B
jB
hsB
l�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
o�B
q�B
q�B
r�B
s�B
r�B
s�B
s�B
t�B
t�B
s�B
s�B
r�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
s�B
r�B
q�B
r�B
r�B
r�B
u�B
u�B
u�B
v�B
v�B
t�B
v�B
u�B
t�B
r�B
t�B
v�B
v�B
v�B
u�B
t�B
u�B
v�B
w�B
w�B
w�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
x�B
y�B
z�B
z�B
z�B
y�B
y�B
{�B
{�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
|�B
|�B
z�B
|�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�%B
�%B
�B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�1B
�1B
�1B
�%B
�B
�7B
�1B
�1B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�DB
�=B
�JB
�VB
�VB
�VB
�VB
�bB
�bB
�bB
�bB
�\B
�\B
�bB
�hB
�hB
�hB
�hB
�bB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�uB
�uB
�{B
�{B
�{B
�uB
�uB
�{B
�{B
�{B
�{B
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
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	}�B	z�B	��B	�!B	�;B
%B
!�B
u�B
�sBG�B�{B��B�!B�9B�B�wB�dB�}B�jB�BK�BP�BVBW
BJ�B=qB-B+B?}B5?B6FB,B(�B�B#�BPB�B�
B��Bv�Bv�BbNBR�B9XB(�B�B
��B
�B
�HB
��B
�VB
�JB
u�B
hsB
XB
7LB
)�B
�B	��B	B	�B	z�B	YB	/B	/B	0!B	�B��B	B��B��B	DB	B��B	DB	�B	"�B	49B	O�B	XB	W
B	W
B	XB	VB	R�B	ZB	_;B	`BB	dZB	cTB	aHB	[#B	Q�B	`BB	YB	I�B	T�B	iyB	l�B	�oB	��B	�!B	�B	�FB	ŢB	�
B	�B	�B	��B	��B	��B
B
B

=B
+B
B
bB
�B
 �B
"�B
"�B
 �B
#�B
$�B
&�B
'�B
%�B
$�B
%�B
&�B
$�B
!�B
�B
 �B
%�B
$�B
!�B
�B
�B
�B
 �B
�B
"�B
#�B
�B
�B
 �B
 �B
#�B
(�B
)�B
'�B
$�B
�B
�B
DB	��B
  B
%B
�B
�B
&�B
%�B
 �B
�B
�B
�B
�B
!�B
�B
�B
�B
�B
'�B
,B
(�B
!�B
�B
�B
�B
!�B
"�B
 �B
�B
!�B
�B
 �B
"�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
bB
�B
�B
�B
�B
{B
oB
\B
VB
JB

=B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B
B
+B
B	��B
B
+B
%B
%B
B
B
1B
+B
+B
%B
B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B	��B	��B
  B	��B
B
B
B
B
B	��B
B
B
B	��B	��B	��B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B
  B
B
%B
B
%B
B
B
B
%B
1B
1B
DB
JB
DB

=B
JB
JB
DB
PB
VB
VB
DB
bB
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
"�B
#�B
#�B
 �B
#�B
$�B
$�B
"�B
 �B
#�B
#�B
$�B
$�B
%�B
&�B
'�B
&�B
&�B
%�B
%�B
'�B
'�B
'�B
'�B
'�B
&�B
'�B
%�B
%�B
$�B
#�B
$�B
$�B
#�B
"�B
!�B
�B
 �B
#�B
)�B
)�B
+B
+B
)�B
)�B
'�B
'�B
,B
-B
-B
-B
-B
,B
0!B
0!B
/B
/B
.B
0!B
0!B
0!B
0!B
0!B
.B
.B
.B
0!B
2-B
2-B
33B
2-B
2-B
1'B
/B
/B
1'B
2-B
33B
33B
33B
33B
1'B
/B
.B
2-B
1'B
49B
5?B
49B
6FB
49B
49B
49B
5?B
49B
6FB
5?B
6FB
8RB
9XB
8RB
8RB
7LB
8RB
8RB
9XB
8RB
7LB
5?B
8RB
;dB
:^B
9XB
:^B
;dB
:^B
8RB
:^B
<jB
<jB
<jB
=qB
>wB
>wB
=qB
;dB
;dB
=qB
?}B
?}B
>wB
>wB
=qB
<jB
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
D�B
C�B
C�B
C�B
D�B
D�B
D�B
B�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
B�B
A�B
D�B
G�B
G�B
G�B
F�B
F�B
H�B
G�B
G�B
F�B
D�B
E�B
F�B
F�B
G�B
F�B
F�B
H�B
I�B
H�B
I�B
H�B
H�B
I�B
I�B
I�B
H�B
G�B
H�B
H�B
G�B
H�B
G�B
K�B
L�B
L�B
M�B
N�B
N�B
O�B
N�B
O�B
O�B
N�B
N�B
N�B
O�B
P�B
P�B
R�B
S�B
T�B
W
B
W
B
W
B
W
B
XB
YB
XB
W
B
VB
W
B
XB
XB
W
B
XB
YB
ZB
ZB
ZB
YB
YB
ZB
ZB
YB
YB
[#B
[#B
ZB
YB
YB
[#B
ZB
ZB
YB
YB
ZB
ZB
ZB
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
]/B
]/B
]/B
\)B
]/B
^5B
_;B
_;B
^5B
]/B
]/B
\)B
[#B
[#B
\)B
aHB
bNB
aHB
aHB
`BB
_;B
\)B
]/B
_;B
_;B
aHB
aHB
bNB
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
bNB
aHB
`BB
bNB
aHB
cTB
ffB
ffB
ffB
e`B
dZB
gmB
gmB
ffB
gmB
gmB
gmB
e`B
e`B
gmB
ffB
ffB
iyB
iyB
iyB
hsB
ffB
gmB
iyB
iyB
hsB
iyB
hsB
iyB
l�B
l�B
l�B
l�B
l�B
k�B
jB
hsB
l�B
m�B
m�B
m�B
m�B
n�B
n�B
m�B
m�B
o�B
o�B
o�B
n�B
n�B
o�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
o�B
q�B
q�B
r�B
s�B
r�B
s�B
s�B
t�B
t�B
s�B
s�B
r�B
s�B
s�B
s�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
s�B
r�B
q�B
r�B
r�B
r�B
u�B
u�B
u�B
v�B
v�B
t�B
v�B
u�B
t�B
r�B
t�B
v�B
v�B
v�B
u�B
t�B
u�B
v�B
w�B
w�B
w�B
v�B
v�B
w�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
x�B
x�B
y�B
z�B
z�B
z�B
y�B
y�B
{�B
{�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
}�B
}�B
}�B
}�B
|�B
|�B
z�B
|�B
~�B
~�B
~�B
~�B
~�B
~�B
}�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�B
�%B
�%B
�B
�%B
�%B
�%B
�%B
�%B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�1B
�1B
�1B
�%B
�B
�7B
�1B
�1B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�DB
�=B
�JB
�VB
�VB
�VB
�VB
�bB
�bB
�bB
�bB
�\B
�\B
�bB
�hB
�hB
�hB
�hB
�bB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�uB
�uB
�{B
�{B
�{B
�uB
�uB
�{B
�{B
�{B
�{B
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
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-.00 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230131080126                              AO  ARCAADJP                                                                    20230131080126    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230131080126  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230131080126  QCF$                G�O�G�O�G�O�0               