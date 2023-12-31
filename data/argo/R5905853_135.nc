CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-10-20T09:47:48Z creation;2022-10-20T09:47:49Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221020094748  20221020100210  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @���}X^1   @����ʆ@/��S���cL���F1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�33B�ffB���B�33B���B���B�  B�33B���B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�33B�ffB���B�33B���B���B�  B�33B���B�  B�  B�33B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33C �C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A݄�A݌JA�~�A�MjAܙ�A�]/A�OA�E�A�=qA�5�A�2�A�/�A�)�A�$�A�!�A�xA�hA��A��QA�ȀAۺ^A۰�AۥAۑ�A�c�Aں^A�#nA���AپAٲaAٜCAق�A�ZA�"�A�خA�/�A���A���A���A�F�AʛqAǤ�A�� A��A�	�AëA��A��/A��bA�xA��A� 4A��HA�j�A�"�A��A���A���A�_A�E9A���A��A�A�eA�X�A�'RA�+A���A�$tA�b�A�[�A�jKA��A���A�V�A���A��nA�e�A��WA�1A��A��4A�TaA��A~҉Az��Aw6�At�XAo�{An)_Am?�Al �Aj\�Ahd�Af��Ae'�Ab�NA_)_A]MAZ�BAY�zAU�*AQYAL��AK�jAJoAG�4AE��AB��AA�A>�fA<�vA<!-A9�A8	A6�)A6�A4�A3�)A/��A,��A+^5A*�A*��A)[WA(��A&��A&MA%�BA%u%A%0�A$2�A"�A!��A ($A�cA6�A��A��A_A�AFtAxAq�A�A�A�WA�yAhsA�A��ADgA�A0�AbNA:�A�A��A�+AeA��A�A�A~A�PA.IA��A�A.�A�?AsA_A�wAR�A/�Ac A;A�A)�A
�A
��A
2�A	��A	L0A�nA��As�A8�A��A��AQ�A��A0�A��A��AGA�Ae�AiD@��@���@���@��k@���@���@�]�@���@�@@��+@�`�@�Q@�@�@�9X@���@��c@�@�z�@��o@��p@�r�@�@�t@���@��o@�t�@��?@�$�@�K�@�n�@镁@�!@��o@�c�@� \@��@�bN@�GE@��@�Y@��P@�֡@�_�@�1@��@�@@�F�@��@���@�N�@�G@�=@�?}@��'@�6�@�˒@ߢ�@�P�@�4�@��@މ�@��@���@ܴ9@ܖ�@�-�@��z@�dZ@�Ɇ@�H�@�s�@�(�@��@��@֧�@�O�@�7�@��@ӑh@��@�9X@Ѣ�@�	l@�6@�n/@�V@΅�@�,=@��@�&�@̉�@��@��6@�*0@��H@ʗ�@�;�@���@ɪ�@ɋ�@�s�@Ȼ�@�A�@�ԕ@Ǹ�@ǨX@ǐ�@�}�@��f@ƚ@�bN@�9X@�� @�@��]@�PH@��#@�^�@��|@ªe@�-@�c@��P@���@�h�@��@���@���@�f�@�Dg@��K@�Q@���@���@��@��@��@�l�@�!@��@�+@��}@���@���@�m�@�\�@�C-@�*�@�@���@���@���@�ݘ@��X@�1�@���@�D�@��@�R�@�b@��&@���@�C@�ߤ@���@�>B@��9@�e,@���@�� @�}V@�{@�+�@���@���@�A�@��9@��"@��f@�5?@�!-@��@���@���@�?@���@�a�@��.@�J@��S@��@��@�@�@��*@�&@���@�($@���@�(�@��@��@��*@���@�D�@�Xy@�kQ@�_@�/�@��g@���@��@�/�@���@��)@��@��@���@�'R@��@�?}@��h@�c�@�I�@�!@��T@��0@�ƨ@���@�=q@�8�@���@���@���@���@�w2@�&@�ی@�Ɇ@���@���@�u�@�]d@���@��q@���@�\�@�6z@� \@�o@��@���@��@���@���@���@�`�@�Ov@�:�@���@���@�|�@�O@�@�S@��c@�xl@�@�J@���@��9@�e,@� i@��@��W@��@��$@���@�j�@�$t@��2@���@�&�@�ԕ@��[@��k@�rG@�0�@���@��z@�'R@�_@��@���@���@��0@���@�J#@���@�ی@��z@�:*@�u@��@��@�Z�@�&�@���@���@���@�W�@�b@��*@�]�@�!-@��@��o@�A�@�7@���@��w@�j�@��@��@�kQ@�'R@���@��N@���@��@�\�@�&@���@���@�c @�&�@�	@��@���@�.I@�͟@��Y@�:*@�M@��@��@���@�o @�C@���@�i�@�$@��+@��@���@��4@�A�@��$@��L@��@�d�@��@��@��k@�a�@�F@�+@��@���@�:*@�0U@�
@A�@~��@~��@~4@}�-@}2a@}	l@|tT@{�&@{e�@{1�@{�@{�@zz@z?@y�@y��@y�@x�.@x �@w�]@wg�@w@O@v�,@vR�@u��@uY�@t��@t�@t?�@s��@st�@s�@r��@r!�@q�C@qu�@qF@q	l@p�[@p�@p6@o�@o��@n�M@n}V@ne@m�C@m4@l�$@l[�@k�m@ka@j�@j5?@i��@i+@hی@h��@h�@g��@gS@f��@f �@e�~@erG@e!�@d�j@d_@c��@b�c@b�F@bE�@b{@a|@`��@`-�@_�@_�*@^�,@^�1@^E�@^O@]�)@]�^@]p�@\�v@\�@[iD@Z�c@Z�}@Z��@Z&�@Y�N@Y�@Y4@X�@XH@W�A@W�;@W�
@W��@V͟@U��@U�@UT�@U�@T֡@T��@T��@Tr�@T6@St�@S=@SS@R6�@R�@Q��@Q^�@Q�@Pی@P�z@P�@O��@N�"@NTa@N=q@N+k@N�@N_@M��@M��@M}�@M5�@L�@L(�@Kl�@J�]@J	@I�@I�M@Iu�@IVm@H��@H��@H|�@Hw�@HK^@G�
@G��@G��@G��@G)_@F�s@F��@F1�@E�@E�7@EO�@E�@D�[@DPH@C�]@C�@C33@Bߤ@B��@Bxl@B8�@B_@A��@A%F@@��@@�u@?��@?�@?>�@?C@>��@>��@>h
@>.�@=�^@=��@=o @=\�@=/@<��@<Q�@;�@;��@;&@:�F@:-@:@: �@9�@9��@9��@9<6@9(�@9�@8�9@8_@7��@7e�@7Y@6��@6�@6�@6��@6�@6�,@6l�@6-@6
�@5�)@5rG@5%@4�?@4��@4I�@4M@3��@3��@3a@2��@2�F@2H�@2+k@1��@1��@1��@1w2@1e,@1-w@0�f@0�/@0��@0bN@/�A@/��@/��@/a@/�@.҉@.��@.�1@.^5@.�@-��@-zx@-B�@-*0@,�@,��@,	�@+��@+dZ@+@O@+$t@*��@*��@*q�@*#:@)�@)zx@)4@)V@(�z@(Ft@($@(�@'�&@'v`@'9�@''�@'�@&�<@&��@&��@&a|@&.�@%��@%@%�@%G�@$�v@$��@$tT@$Z@$G@#�@#j�@#+@"�@"�@"��@"a|@"$�@"_@!ϫ@!��@!�~@!a�@!=�@!#�@ ��@ ��@ ��@ 9X@ƨ@��@�	@{J@O@��@��@��@h
@�@�@�T@�@�9@��@�@��@T�@5�@�@�@�@�$@�.@oi@I�@$@  @�g@�@$t@��@�B@��@6�@�@�H@�~@s�@F@�@��@K^@M@�g@�a@�[@�:@o�@8@�@��@ȴ@�@i�@;�@�@��@�@�@�^@J�@��@��@�_@]d@-�@�@��@~�@j�@U�@>�@6z@�@��@�@��@�F@E�@	@�T@��@�@c�@!�@��@�D@oi@D�@� @�@n/@o@�@͟@��@kQ@@�@�@�z@��@zx@<6@��@��@��@_@2�@1@�}@��@g�@X�@�@
��@
��@
�6@
{�@
YK@
Ta@
L0@
e@
_@	��@	��@	�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A݄�A݌JA�~�A�MjAܙ�A�]/A�OA�E�A�=qA�5�A�2�A�/�A�)�A�$�A�!�A�xA�hA��A��QA�ȀAۺ^A۰�AۥAۑ�A�c�Aں^A�#nA���AپAٲaAٜCAق�A�ZA�"�A�خA�/�A���A���A���A�F�AʛqAǤ�A�� A��A�	�AëA��A��/A��bA�xA��A� 4A��HA�j�A�"�A��A���A���A�_A�E9A���A��A�A�eA�X�A�'RA�+A���A�$tA�b�A�[�A�jKA��A���A�V�A���A��nA�e�A��WA�1A��A��4A�TaA��A~҉Az��Aw6�At�XAo�{An)_Am?�Al �Aj\�Ahd�Af��Ae'�Ab�NA_)_A]MAZ�BAY�zAU�*AQYAL��AK�jAJoAG�4AE��AB��AA�A>�fA<�vA<!-A9�A8	A6�)A6�A4�A3�)A/��A,��A+^5A*�A*��A)[WA(��A&��A&MA%�BA%u%A%0�A$2�A"�A!��A ($A�cA6�A��A��A_A�AFtAxAq�A�A�A�WA�yAhsA�A��ADgA�A0�AbNA:�A�A��A�+AeA��A�A�A~A�PA.IA��A�A.�A�?AsA_A�wAR�A/�Ac A;A�A)�A
�A
��A
2�A	��A	L0A�nA��As�A8�A��A��AQ�A��A0�A��A��AGA�Ae�AiD@��@���@���@��k@���@���@�]�@���@�@@��+@�`�@�Q@�@�@�9X@���@��c@�@�z�@��o@��p@�r�@�@�t@���@��o@�t�@��?@�$�@�K�@�n�@镁@�!@��o@�c�@� \@��@�bN@�GE@��@�Y@��P@�֡@�_�@�1@��@�@@�F�@��@���@�N�@�G@�=@�?}@��'@�6�@�˒@ߢ�@�P�@�4�@��@މ�@��@���@ܴ9@ܖ�@�-�@��z@�dZ@�Ɇ@�H�@�s�@�(�@��@��@֧�@�O�@�7�@��@ӑh@��@�9X@Ѣ�@�	l@�6@�n/@�V@΅�@�,=@��@�&�@̉�@��@��6@�*0@��H@ʗ�@�;�@���@ɪ�@ɋ�@�s�@Ȼ�@�A�@�ԕ@Ǹ�@ǨX@ǐ�@�}�@��f@ƚ@�bN@�9X@�� @�@��]@�PH@��#@�^�@��|@ªe@�-@�c@��P@���@�h�@��@���@���@�f�@�Dg@��K@�Q@���@���@��@��@��@�l�@�!@��@�+@��}@���@���@�m�@�\�@�C-@�*�@�@���@���@���@�ݘ@��X@�1�@���@�D�@��@�R�@�b@��&@���@�C@�ߤ@���@�>B@��9@�e,@���@�� @�}V@�{@�+�@���@���@�A�@��9@��"@��f@�5?@�!-@��@���@���@�?@���@�a�@��.@�J@��S@��@��@�@�@��*@�&@���@�($@���@�(�@��@��@��*@���@�D�@�Xy@�kQ@�_@�/�@��g@���@��@�/�@���@��)@��@��@���@�'R@��@�?}@��h@�c�@�I�@�!@��T@��0@�ƨ@���@�=q@�8�@���@���@���@���@�w2@�&@�ی@�Ɇ@���@���@�u�@�]d@���@��q@���@�\�@�6z@� \@�o@��@���@��@���@���@���@�`�@�Ov@�:�@���@���@�|�@�O@�@�S@��c@�xl@�@�J@���@��9@�e,@� i@��@��W@��@��$@���@�j�@�$t@��2@���@�&�@�ԕ@��[@��k@�rG@�0�@���@��z@�'R@�_@��@���@���@��0@���@�J#@���@�ی@��z@�:*@�u@��@��@�Z�@�&�@���@���@���@�W�@�b@��*@�]�@�!-@��@��o@�A�@�7@���@��w@�j�@��@��@�kQ@�'R@���@��N@���@��@�\�@�&@���@���@�c @�&�@�	@��@���@�.I@�͟@��Y@�:*@�M@��@��@���@�o @�C@���@�i�@�$@��+@��@���@��4@�A�@��$@��L@��@�d�@��@��@��k@�a�@�F@�+@��@���@�:*@�0U@�
@A�@~��@~��@~4@}�-@}2a@}	l@|tT@{�&@{e�@{1�@{�@{�@zz@z?@y�@y��@y�@x�.@x �@w�]@wg�@w@O@v�,@vR�@u��@uY�@t��@t�@t?�@s��@st�@s�@r��@r!�@q�C@qu�@qF@q	l@p�[@p�@p6@o�@o��@n�M@n}V@ne@m�C@m4@l�$@l[�@k�m@ka@j�@j5?@i��@i+@hی@h��@h�@g��@gS@f��@f �@e�~@erG@e!�@d�j@d_@c��@b�c@b�F@bE�@b{@a|@`��@`-�@_�@_�*@^�,@^�1@^E�@^O@]�)@]�^@]p�@\�v@\�@[iD@Z�c@Z�}@Z��@Z&�@Y�N@Y�@Y4@X�@XH@W�A@W�;@W�
@W��@V͟@U��@U�@UT�@U�@T֡@T��@T��@Tr�@T6@St�@S=@SS@R6�@R�@Q��@Q^�@Q�@Pی@P�z@P�@O��@N�"@NTa@N=q@N+k@N�@N_@M��@M��@M}�@M5�@L�@L(�@Kl�@J�]@J	@I�@I�M@Iu�@IVm@H��@H��@H|�@Hw�@HK^@G�
@G��@G��@G��@G)_@F�s@F��@F1�@E�@E�7@EO�@E�@D�[@DPH@C�]@C�@C33@Bߤ@B��@Bxl@B8�@B_@A��@A%F@@��@@�u@?��@?�@?>�@?C@>��@>��@>h
@>.�@=�^@=��@=o @=\�@=/@<��@<Q�@;�@;��@;&@:�F@:-@:@: �@9�@9��@9��@9<6@9(�@9�@8�9@8_@7��@7e�@7Y@6��@6�@6�@6��@6�@6�,@6l�@6-@6
�@5�)@5rG@5%@4�?@4��@4I�@4M@3��@3��@3a@2��@2�F@2H�@2+k@1��@1��@1��@1w2@1e,@1-w@0�f@0�/@0��@0bN@/�A@/��@/��@/a@/�@.҉@.��@.�1@.^5@.�@-��@-zx@-B�@-*0@,�@,��@,	�@+��@+dZ@+@O@+$t@*��@*��@*q�@*#:@)�@)zx@)4@)V@(�z@(Ft@($@(�@'�&@'v`@'9�@''�@'�@&�<@&��@&��@&a|@&.�@%��@%@%�@%G�@$�v@$��@$tT@$Z@$G@#�@#j�@#+@"�@"�@"��@"a|@"$�@"_@!ϫ@!��@!�~@!a�@!=�@!#�@ ��@ ��@ ��@ 9X@ƨ@��@�	@{J@O@��@��@��@h
@�@�@�T@�@�9@��@�@��@T�@5�@�@�@�@�$@�.@oi@I�@$@  @�g@�@$t@��@�B@��@6�@�@�H@�~@s�@F@�@��@K^@M@�g@�a@�[@�:@o�@8@�@��@ȴ@�@i�@;�@�@��@�@�@�^@J�@��@��@�_@]d@-�@�@��@~�@j�@U�@>�@6z@�@��@�@��@�F@E�@	@�T@��@�@c�@!�@��@�D@oi@D�@� @�@n/@o@�@͟@��@kQ@@�@�@�z@��@zx@<6@��@��@��@_@2�@1@�}@��@g�@X�@�@
��@
��@
�6@
{�@
YK@
Ta@
L0@
e@
_@	��@	��@	�h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
ݘB
�IB
�/B
�#B
�	B
��B
��B
��B
ؓB
ؓB
خB
��B
��B
��B
��B
��B
��B
��B
خB
�+B
�EB
��B
�eB
ٚB
�B
�]B
�~B
�OB
�B
��B
��B
�wB
�B
�*B
�B
� B
��B
�YB
�B
�fB
�BB	B�B(�BFBiB�B�hB��BUBmB�cB�B�&B��B��BzDBn�Bk�Be,B[�BO�B<jB�B
��B4B�%B��Bs�BVmB:�B&fBmB
�B
ɆB
��B
�7B
v`B
j�B
Z7B
6�B
�B
%B	�zB	�B	ѝB	��B	� B	��B	�{B	��B	��B	zB	oB	g8B	ZQB	G_B	<�B	.B	%`B	B�6B�B�B�B�!B�BՁB҉B�BؓBڠB�BB�BB�B�B��B�B�	B� B��BοB�HBԕB�B��B�fB�B��B	3B	 �B	5�B	C�B	;dB	&�B	"hB	�B	)B	?B	�B	0B	�B	bB	"4B	6�B	4�B	8B	=<B	8RB	D3B	jB	~�B	�_B	�B	��B	�~B	�hB	��B	��B	��B	ҽB	�B	��B	��B	�CB	��B	޸B	��B	��B	�B	��B	��B	�mB	�B	�0B	�xB	��B	��B	��B	ŢB	�mB	ÖB	�_B	�EB	��B	�rB	��B	ɺB	��B	�B	�+B	ŢB	��B	��B	�HB	��B	�>B	�?B	�KB	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�8B	�8B	��B	��B	�RB	��B	��B	�6B	��B	��B	�B	�tB	�GB	��B	�%B	��B	�9B	��B	��B	��B	��B	��B	�|B	�?B	��B	�*B	��B	��B	�B	��B	��B	�}B	�4B	�iB	�UB	�B	�9B	�SB	ƎB	�EB	�KB	ȚB	ɺB	�B	��B	��B	��B	�B	�6B	�jB	�\B	�}B	��B	�B	��B	өB	�,B	�B	�{B	�MB	�2B	ԕB	՛B	՛B	�9B	�SB	�9B	�SB	��B	�$B	ևB	��B	��B	��B	��B	��B	�B	�B	�B	�7B	��B	ںB	ںB	�	B	�=B	�qB	�)B	ܬB	ܒB	��B	�B	ބB	��B	�;B	�;B	�VB	�;B	�BB	�vB	��B	�B	�-B	�B	��B	��B	�hB	�B	�FB	�`B	�B	�_B	�QB	�B	�B	�B	�B	�WB	�B	��B	�)B	��B	��B	��B	�B	�!B	�B	��B	�'B	��B	��B	�B	��B	�TB	��B	�B	�tB	��B	�B	�+B	�FB	�`B	�`B	�LB	�	B	�^B	�rB	��B	�0B	�JB	�B	�PB	�jB	�B	�B	�6B	��B	�qB	��B	�VB	�]B	��B	�(B	�qB	��B	��B	�B	��B	�<B	�B	��B	�JB	�VB	�"B	�VB	��B	��B	�qB	��B	�<B	�B	�BB	��B	��B	��B
 OB
  B	��B	��B
 4B
 iB
 iB
�B
�B

rB
�B
�B
�B
�B
(B
vB
bB
�B
�B
NB
�B
�B
B
oB
4B
�B
�B
\B
vB
�B
�B
�B
�B
B
mB
�B
�B
SB
$B
YB
�B
�B
�B
sB
sB
sB
YB
EB
B
B
B
�B
�B
�B
YB
�B
�B
?B
B
+B
yB
yB
�B
�B
B
�B
1B
�B
B
�B
kB
7B
�B
kB
�B
�B
�B
)B
�B
xB
�B
]B
�B
�B
�B
~B
B
OB
�B
jB
�B
�B
B
VB
 BB
 'B
 'B
 'B
 'B
 BB
 \B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"hB
"�B
"�B
"�B
"�B
"�B
# B
#�B
$@B
$tB
$�B
$�B
%`B
%zB
%�B
%�B
%�B
&B
&�B
&�B
'B
'mB
'�B
'�B
'�B
'�B
($B
(>B
(�B
(�B
)�B
*B
*eB
*�B
*�B
*B
*�B
+B
+6B
+�B
,"B
,qB
,�B
,�B
-CB
.B
.}B
.�B
/ B
/B
/5B
/�B
0;B
0�B
1AB
1vB
1�B
1�B
1�B
2GB
2�B
3B
33B
3MB
3�B
3�B
3�B
4�B
4�B
5B
5�B
5�B
5�B
5�B
5�B
6B
6zB
6�B
6�B
6�B
6�B
7B
7B
7LB
7�B
8B
8�B
8�B
8�B
9$B
8�B
9	B
9XB
9�B
9�B
:B
:DB
:xB
:�B
:xB
:�B
:�B
;0B
;�B
;�B
;�B
<B
<6B
<�B
<�B
<�B
=B
=qB
=�B
=�B
=�B
=�B
>]B
>]B
>�B
?.B
@B
@�B
A�B
A�B
A�B
A�B
BuB
CB
C�B
D3B
EB
EB
EmB
FB
FtB
FtB
F�B
FtB
F�B
F�B
F�B
F�B
FYB
F%B
E�B
FB
F�B
F�B
GEB
GB
GzB
G�B
G�B
HKB
IRB
IlB
J#B
J#B
J=B
J�B
J�B
J�B
K^B
K�B
LB
L�B
L�B
L�B
L�B
MPB
M�B
M�B
NB
N<B
NpB
N�B
N�B
N�B
N�B
OBB
O\B
O\B
PB
O�B
O�B
PbB
P�B
P�B
P�B
P�B
QhB
Q�B
Q�B
Q�B
Q�B
RB
R B
R:B
R�B
SB
S&B
S�B
S�B
S�B
TB
T{B
T�B
U2B
UMB
UMB
U�B
U�B
VB
U�B
VB
V�B
VmB
VmB
VmB
V�B
W
B
W?B
WYB
W�B
XB
X+B
X_B
XyB
X�B
X�B
Y1B
YKB
YeB
YB
Y�B
YeB
YB
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
[=B
[WB
[�B
[�B
\B
\B
\B
\)B
\�B
\�B
]B
\�B
]IB
]�B
^B
^B
^B
^B
^B
^5B
^�B
^�B
^�B
_;B
_�B
`B
`�B
`�B
a�B
a�B
a�B
a�B
bB
bhB
cB
c B
c B
c B
c:B
b�B
b�B
b�B
cB
cTB
cnB
cnB
c�B
c�B
dZB
d�B
d�B
e,B
e�B
f2B
f2B
fLB
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
h$B
hXB
h�B
h�B
h�B
iB
i_B
iyB
i�B
i�B
i�B
j0B
jKB
j�B
j�B
kB
k6B
kB
k�B
k�B
k�B
k�B
l"B
lqB
l�B
l�B
l�B
mCB
mCB
m]B
m]B
m�B
m�B
nB
n/B
ncB
n}B
n�B
n�B
n�B
n�B
oB
oOB
o�B
o�B
p!B
p!B
p!B
p�B
p�B
p�B
q'B
qAB
qAB
q�B
q�B
q�B
q�B
q�B
r-B
rGB
raB
r�B
r�B
r�B
sB
sB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
u?B
uZB
uZB
uZB
uZB
utB
utB
u�B
u�B
u�B
v+B
v+B
v`B
vzB
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
xB
w�B
xlB
x�B
x�B
y$B
y$B
y>B
y>B
yrB
y�B
y�B
y�B
y�B
zB
zB
z*B
zDB
z�B
z�B
z�B
z�B
{0B
{dB
{�B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
}B
}<B
}VB
}�B
}�B
}�B
~B
~(B
~BB
~BB
~BB
~wB
~�B
~�B
~�B
.B
HB
cB
�B
�B
�B
�4B
��B
��B
�B
�;B
��B
��B
��B
�[B
�[B
�uB
��B
��B
�-B
��B
��B
��B
��B
�3B
�3B
�MB
��B
�B
�mB
�mB
��B
�B
�B
�%B
�%B
�?B
�?B
��B
�B
��B
��B
��B
�_B
�EB
��B
�zB
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
ݘB
�IB
�/B
�#B
�	B
��B
��B
��B
ؓB
ؓB
خB
��B
��B
��B
��B
��B
��B
��B
خB
�+B
�EB
��B
�eB
ٚB
�B
�]B
�~B
�OB
�B
��B
��B
�wB
�B
�*B
�B
� B
��B
�YB
�B
�fB
�BB	B�B(�BFBiB�B�hB��BUBmB�cB�B�&B��B��BzDBn�Bk�Be,B[�BO�B<jB�B
��B4B�%B��Bs�BVmB:�B&fBmB
�B
ɆB
��B
�7B
v`B
j�B
Z7B
6�B
�B
%B	�zB	�B	ѝB	��B	� B	��B	�{B	��B	��B	zB	oB	g8B	ZQB	G_B	<�B	.B	%`B	B�6B�B�B�B�!B�BՁB҉B�BؓBڠB�BB�BB�B�B��B�B�	B� B��BοB�HBԕB�B��B�fB�B��B	3B	 �B	5�B	C�B	;dB	&�B	"hB	�B	)B	?B	�B	0B	�B	bB	"4B	6�B	4�B	8B	=<B	8RB	D3B	jB	~�B	�_B	�B	��B	�~B	�hB	��B	��B	��B	ҽB	�B	��B	��B	�CB	��B	޸B	��B	��B	�B	��B	��B	�mB	�B	�0B	�xB	��B	��B	��B	ŢB	�mB	ÖB	�_B	�EB	��B	�rB	��B	ɺB	��B	�B	�+B	ŢB	��B	��B	�HB	��B	�>B	�?B	�KB	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�8B	�8B	��B	��B	�RB	��B	��B	�6B	��B	��B	�B	�tB	�GB	��B	�%B	��B	�9B	��B	��B	��B	��B	��B	�|B	�?B	��B	�*B	��B	��B	�B	��B	��B	�}B	�4B	�iB	�UB	�B	�9B	�SB	ƎB	�EB	�KB	ȚB	ɺB	�B	��B	��B	��B	�B	�6B	�jB	�\B	�}B	��B	�B	��B	өB	�,B	�B	�{B	�MB	�2B	ԕB	՛B	՛B	�9B	�SB	�9B	�SB	��B	�$B	ևB	��B	��B	��B	��B	��B	�B	�B	�B	�7B	��B	ںB	ںB	�	B	�=B	�qB	�)B	ܬB	ܒB	��B	�B	ބB	��B	�;B	�;B	�VB	�;B	�BB	�vB	��B	�B	�-B	�B	��B	��B	�hB	�B	�FB	�`B	�B	�_B	�QB	�B	�B	�B	�B	�WB	�B	��B	�)B	��B	��B	��B	�B	�!B	�B	��B	�'B	��B	��B	�B	��B	�TB	��B	�B	�tB	��B	�B	�+B	�FB	�`B	�`B	�LB	�	B	�^B	�rB	��B	�0B	�JB	�B	�PB	�jB	�B	�B	�6B	��B	�qB	��B	�VB	�]B	��B	�(B	�qB	��B	��B	�B	��B	�<B	�B	��B	�JB	�VB	�"B	�VB	��B	��B	�qB	��B	�<B	�B	�BB	��B	��B	��B
 OB
  B	��B	��B
 4B
 iB
 iB
�B
�B

rB
�B
�B
�B
�B
(B
vB
bB
�B
�B
NB
�B
�B
B
oB
4B
�B
�B
\B
vB
�B
�B
�B
�B
B
mB
�B
�B
SB
$B
YB
�B
�B
�B
sB
sB
sB
YB
EB
B
B
B
�B
�B
�B
YB
�B
�B
?B
B
+B
yB
yB
�B
�B
B
�B
1B
�B
B
�B
kB
7B
�B
kB
�B
�B
�B
)B
�B
xB
�B
]B
�B
�B
�B
~B
B
OB
�B
jB
�B
�B
B
VB
 BB
 'B
 'B
 'B
 'B
 BB
 \B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
"hB
"�B
"�B
"�B
"�B
"�B
# B
#�B
$@B
$tB
$�B
$�B
%`B
%zB
%�B
%�B
%�B
&B
&�B
&�B
'B
'mB
'�B
'�B
'�B
'�B
($B
(>B
(�B
(�B
)�B
*B
*eB
*�B
*�B
*B
*�B
+B
+6B
+�B
,"B
,qB
,�B
,�B
-CB
.B
.}B
.�B
/ B
/B
/5B
/�B
0;B
0�B
1AB
1vB
1�B
1�B
1�B
2GB
2�B
3B
33B
3MB
3�B
3�B
3�B
4�B
4�B
5B
5�B
5�B
5�B
5�B
5�B
6B
6zB
6�B
6�B
6�B
6�B
7B
7B
7LB
7�B
8B
8�B
8�B
8�B
9$B
8�B
9	B
9XB
9�B
9�B
:B
:DB
:xB
:�B
:xB
:�B
:�B
;0B
;�B
;�B
;�B
<B
<6B
<�B
<�B
<�B
=B
=qB
=�B
=�B
=�B
=�B
>]B
>]B
>�B
?.B
@B
@�B
A�B
A�B
A�B
A�B
BuB
CB
C�B
D3B
EB
EB
EmB
FB
FtB
FtB
F�B
FtB
F�B
F�B
F�B
F�B
FYB
F%B
E�B
FB
F�B
F�B
GEB
GB
GzB
G�B
G�B
HKB
IRB
IlB
J#B
J#B
J=B
J�B
J�B
J�B
K^B
K�B
LB
L�B
L�B
L�B
L�B
MPB
M�B
M�B
NB
N<B
NpB
N�B
N�B
N�B
N�B
OBB
O\B
O\B
PB
O�B
O�B
PbB
P�B
P�B
P�B
P�B
QhB
Q�B
Q�B
Q�B
Q�B
RB
R B
R:B
R�B
SB
S&B
S�B
S�B
S�B
TB
T{B
T�B
U2B
UMB
UMB
U�B
U�B
VB
U�B
VB
V�B
VmB
VmB
VmB
V�B
W
B
W?B
WYB
W�B
XB
X+B
X_B
XyB
X�B
X�B
Y1B
YKB
YeB
YB
Y�B
YeB
YB
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
[=B
[WB
[�B
[�B
\B
\B
\B
\)B
\�B
\�B
]B
\�B
]IB
]�B
^B
^B
^B
^B
^B
^5B
^�B
^�B
^�B
_;B
_�B
`B
`�B
`�B
a�B
a�B
a�B
a�B
bB
bhB
cB
c B
c B
c B
c:B
b�B
b�B
b�B
cB
cTB
cnB
cnB
c�B
c�B
dZB
d�B
d�B
e,B
e�B
f2B
f2B
fLB
f�B
f�B
f�B
f�B
gRB
g�B
g�B
g�B
h$B
hXB
h�B
h�B
h�B
iB
i_B
iyB
i�B
i�B
i�B
j0B
jKB
j�B
j�B
kB
k6B
kB
k�B
k�B
k�B
k�B
l"B
lqB
l�B
l�B
l�B
mCB
mCB
m]B
m]B
m�B
m�B
nB
n/B
ncB
n}B
n�B
n�B
n�B
n�B
oB
oOB
o�B
o�B
p!B
p!B
p!B
p�B
p�B
p�B
q'B
qAB
qAB
q�B
q�B
q�B
q�B
q�B
r-B
rGB
raB
r�B
r�B
r�B
sB
sB
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
u?B
uZB
uZB
uZB
uZB
utB
utB
u�B
u�B
u�B
v+B
v+B
v`B
vzB
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
xB
w�B
xlB
x�B
x�B
y$B
y$B
y>B
y>B
yrB
y�B
y�B
y�B
y�B
zB
zB
z*B
zDB
z�B
z�B
z�B
z�B
{0B
{dB
{�B
{�B
{�B
{�B
{�B
|B
|�B
|�B
|�B
}B
}<B
}VB
}�B
}�B
}�B
~B
~(B
~BB
~BB
~BB
~wB
~�B
~�B
~�B
.B
HB
cB
�B
�B
�B
�4B
��B
��B
�B
�;B
��B
��B
��B
�[B
�[B
�uB
��B
��B
�-B
��B
��B
��B
��B
�3B
�3B
�MB
��B
�B
�mB
�mB
��B
�B
�B
�%B
�%B
�?B
�?B
��B
�B
��B
��B
��B
�_B
�EB
��B
�zB
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221020094700  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20221020094748  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221020094749  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221020094749                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221020184754  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221020184754  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221020100210                      G�O�G�O�G�O�                