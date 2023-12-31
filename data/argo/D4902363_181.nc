CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-11-21T00:35:23Z creation;2017-11-21T00:35:27Z conversion to V3.1;2019-12-19T07:56:12Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20171121003523  20200115121516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_181                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�6�i 1   @�6�    @;���8�Y�dcA��s1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�C3D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��uA��hA��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��7A��A��A�-A�dZA�S�A���A��jA��9A��HA�bA�7LA���A��A��!A���A�9XA���A���A�E�A��RA�z�A�ƨA�bNA�I�A�/A��/A�(�A��/A���A�33A���A�{A�S�A�?}A�1'A���A�hsA��A��yA��DA�I�A���A�Q�A���A���A��A�ĜA��A�C�A�/A��A���A�jA�1A�ƨA�ZA�A~�HA~n�A}��A}�A|n�A{hsAzE�Aw��AuXAs"�Ar1'AqO�Ao��An�Al�jAk�wAk&�AjĜAjz�Ai`BAh1Ag`BAfȴAfM�Ae�#Ad��Ac|�AbȴAbffAa��Aa&�A`�yA_�A^I�A]l�A\�DAZ�jAY7LAX^5AW��AWAV��AV�DAVz�AVQ�AVAU
=AS��ARZAP�RAO�7AN�!AM�AL�AL��ALffAL1'AK��AJ��AI��AH��AGG�AF�\AFM�AE��AD�ACp�ABĜABffAA�AA�A@��A?��A>��A>bA=��A<ĜA< �A:��A:��A:bA9�wA9��A9G�A8��A8��A8A6^5A5`BA5+A5
=A4�`A4��A3�TA3;dA2�A2ZA1ƨA0��A01A/XA.�uA-��A-�PA,�`A,E�A+`BA*�A)p�A(-A'��A'`BA'O�A'7LA&��A&�9A&r�A& �A%�mA%�A%O�A%�A$ĜA$ZA#�A"v�A �A ZA JA�#AhsA�9A�TA��A/A��A`BA�9AS�A�RAQ�A��AE�A��A��A��A?}A9XAx�A�At�A�!A�A&�A
  A��A�+A�AffA(�A��A�uA�DAZA33A�A ��A Z@���@�V@��
@���@��T@�K�@��^@�Ĝ@�1@��
@�
=@�!@�@�w@�\)@��y@�!@�^5@��T@�bN@�^@���@柾@�
=@�V@�O�@�t�@�@��/@���@۶F@�t�@�S�@�o@��H@ڧ�@�{@ؓu@�hs@�Q�@�$�@θR@��@�?}@��`@�z�@�  @���@˾w@�t�@��y@��@ȋD@��@�@�~�@�`B@Ĵ9@�r�@�Z@��
@���@��#@�&�@�Ĝ@�r�@�(�@�S�@�j@���@���@��@���@��@�-@��
@�dZ@�33@��!@�9X@���@�=q@�J@��@��@��@��^@���@��@�"�@�"�@��y@���@���@���@�;d@���@��@��m@���@�|�@��\@�^5@�-@��@���@���@��;@�K�@��R@�p�@�hs@���@��@�G�@�%@���@��P@�ȴ@���@���@�~�@�=q@��#@���@�x�@�X@�G�@�/@��@�V@��`@���@���@��@�A�@���@��;@���@��@�t�@��@�ȴ@��R@��H@��H@��!@�{@�&�@��P@��H@��\@�`B@��D@�/@���@���@�G�@�7L@�1@��-@�%@�1'@��
@�l�@��y@��!@�~�@�=q@�$�@���@��#@��^@��@�I�@���@��@�
=@���@��R@�~�@���@�@���@�p�@�V@��`@�Ĝ@���@�Q�@�1@�;@��@~�R@~ȴ@}�@~{@}O�@{��@{dZ@{C�@z�@z�!@z�\@z~�@z-@y�#@yX@x��@xr�@x  @w��@wl�@w;d@vȴ@v{@u��@up�@u�@u`B@u/@uV@t�@t��@t�D@t9X@sƨ@s�
@t9X@t(�@t�@t(�@t�@t�@s�m@s�F@st�@sdZ@r�H@r�@q�#@q��@qX@p1'@o�@o��@o��@oK�@oK�@nȴ@n@m��@mp�@m/@l��@l��@l��@l�j@l�@l�D@lj@lZ@l(�@l�@k��@k�m@k�F@kdZ@kS�@k33@ko@jn�@i�^@i�@ihs@i%@h�`@h1'@h �@h  @g+@g�@f��@f�y@fv�@f5?@dj@c�m@cC�@b��@b=q@bn�@b~�@b~�@b�\@b�!@b�!@b��@b^5@b�@a��@a��@a��@a��@aG�@aG�@b��@co@co@b�\@b�\@b~�@bM�@a��@a�@`��@_�P@^��@^�@^��@]�@]��@]p�@]?}@\�@\�D@\�D@\�@\��@\j@\(�@[��@[t�@Y�7@Y��@ZM�@Y��@X��@XbN@Xb@Xb@W�@W�w@V��@VV@U�T@U`B@T�@T�@Sƨ@St�@R��@Qx�@PĜ@P�u@Pr�@P1'@O��@O|�@O;d@N�@Nv�@M�h@L��@L��@L(�@K��@K@J��@J~�@J=q@J��@K@J�H@J��@J��@Jn�@I��@I�@H��@HA�@G�;@G�@G\)@FE�@E�@E`B@E?}@E/@D�@D1@C��@C�
@CC�@C"�@C"�@C"�@C"�@Co@B�@B�!@B�!@B~�@Bn�@B^5@BJ@A��@Ax�@@��@@��@@�u@@r�@@b@?|�@?\)@?K�@?;d@?
=@?
=@>��@>�@>�R@>$�@>@=��@=�@=/@<��@<j@<(�@;�
@;t�@;dZ@;C�@;@;o@:��@9�#@9�^@9��@9X@8��@8�`@8��@81'@7��@7l�@7l�@7l�@7;d@6�R@6v�@6V@65?@6{@5�T@5O�@4�/@4�@4j@4(�@3�m@3�F@3�@2�@2�!@2n�@2=q@2-@1��@1�^@1X@1�@0��@0b@0  @/�@/�@/��@/�w@/��@/|�@/;d@.��@.��@.�+@.V@.{@-�-@-�h@-?}@,�@,�@,j@,9X@,(�@,�@+��@+ƨ@+t�@+dZ@+S�@+S�@+33@*�@*n�@)�#@)x�@)hs@)hs@)hs@)X@)�@(��@(Ĝ@(��@(�@(1'@'��@'|�@';d@&�y@&ȴ@&��@&E�@%�@%�@%/@%�@$�/@$��@$(�@#��@#33@"�H@"��@"��@"��@"�!@"��@"~�@"=q@!�^@!�@ ��@ Ĝ@ �u@ �@  �@   @   @�@�;@�w@l�@;d@��@�+@E�@5?@5?@5?@{@�-@O�@?}@��@�@(�@�m@��@t�@S�@C�@o@@�H@~�@�@�#@X@�9@�u@bN@ �@�@l�@\)@K�@�@��@v�@{@�T@�-@�h@�@��@�D@I�@1@�m@ƨ@��@��@t�@S�@o@��@M�@J@��@�#@��@�@��@Ĝ@Ĝ@�9@��@��@�u@�@1'@b@�;@��@�@��@|�@K�@;d@�y@��@ff@E�@5?@@��@O�@V@��@��@j@1@�
@�F@�@dZ@S�@C�@33@"�@o@
��@
�!@
�\@
~�@
n�@
^5@
=q@
�@	��@	�#@	��@	�^@	��@	�7@	x�@	x�@	x�@	X@	G�@��@r�@Q�@1'@ �@b@  @�;@��@�P@�P@|�@�P@�P@�P@l�@K�@
=@v�@$�@�@@�@O�@V@�@�j@z�@I�@��@�
@ƨ@�F@�@S�@33@"�@@�H@��@�\@�\@~�@n�@n�@^5@M�@-@�@��@hs@%@ ��@ ��@ Ĝ@ �@ A�@ 1'@  �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A��uA��hA��uA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��7A��A��A�-A�dZA�S�A���A��jA��9A��HA�bA�7LA���A��A��!A���A�9XA���A���A�E�A��RA�z�A�ƨA�bNA�I�A�/A��/A�(�A��/A���A�33A���A�{A�S�A�?}A�1'A���A�hsA��A��yA��DA�I�A���A�Q�A���A���A��A�ĜA��A�C�A�/A��A���A�jA�1A�ƨA�ZA�A~�HA~n�A}��A}�A|n�A{hsAzE�Aw��AuXAs"�Ar1'AqO�Ao��An�Al�jAk�wAk&�AjĜAjz�Ai`BAh1Ag`BAfȴAfM�Ae�#Ad��Ac|�AbȴAbffAa��Aa&�A`�yA_�A^I�A]l�A\�DAZ�jAY7LAX^5AW��AWAV��AV�DAVz�AVQ�AVAU
=AS��ARZAP�RAO�7AN�!AM�AL�AL��ALffAL1'AK��AJ��AI��AH��AGG�AF�\AFM�AE��AD�ACp�ABĜABffAA�AA�A@��A?��A>��A>bA=��A<ĜA< �A:��A:��A:bA9�wA9��A9G�A8��A8��A8A6^5A5`BA5+A5
=A4�`A4��A3�TA3;dA2�A2ZA1ƨA0��A01A/XA.�uA-��A-�PA,�`A,E�A+`BA*�A)p�A(-A'��A'`BA'O�A'7LA&��A&�9A&r�A& �A%�mA%�A%O�A%�A$ĜA$ZA#�A"v�A �A ZA JA�#AhsA�9A�TA��A/A��A`BA�9AS�A�RAQ�A��AE�A��A��A��A?}A9XAx�A�At�A�!A�A&�A
  A��A�+A�AffA(�A��A�uA�DAZA33A�A ��A Z@���@�V@��
@���@��T@�K�@��^@�Ĝ@�1@��
@�
=@�!@�@�w@�\)@��y@�!@�^5@��T@�bN@�^@���@柾@�
=@�V@�O�@�t�@�@��/@���@۶F@�t�@�S�@�o@��H@ڧ�@�{@ؓu@�hs@�Q�@�$�@θR@��@�?}@��`@�z�@�  @���@˾w@�t�@��y@��@ȋD@��@�@�~�@�`B@Ĵ9@�r�@�Z@��
@���@��#@�&�@�Ĝ@�r�@�(�@�S�@�j@���@���@��@���@��@�-@��
@�dZ@�33@��!@�9X@���@�=q@�J@��@��@��@��^@���@��@�"�@�"�@��y@���@���@���@�;d@���@��@��m@���@�|�@��\@�^5@�-@��@���@���@��;@�K�@��R@�p�@�hs@���@��@�G�@�%@���@��P@�ȴ@���@���@�~�@�=q@��#@���@�x�@�X@�G�@�/@��@�V@��`@���@���@��@�A�@���@��;@���@��@�t�@��@�ȴ@��R@��H@��H@��!@�{@�&�@��P@��H@��\@�`B@��D@�/@���@���@�G�@�7L@�1@��-@�%@�1'@��
@�l�@��y@��!@�~�@�=q@�$�@���@��#@��^@��@�I�@���@��@�
=@���@��R@�~�@���@�@���@�p�@�V@��`@�Ĝ@���@�Q�@�1@�;@��@~�R@~ȴ@}�@~{@}O�@{��@{dZ@{C�@z�@z�!@z�\@z~�@z-@y�#@yX@x��@xr�@x  @w��@wl�@w;d@vȴ@v{@u��@up�@u�@u`B@u/@uV@t�@t��@t�D@t9X@sƨ@s�
@t9X@t(�@t�@t(�@t�@t�@s�m@s�F@st�@sdZ@r�H@r�@q�#@q��@qX@p1'@o�@o��@o��@oK�@oK�@nȴ@n@m��@mp�@m/@l��@l��@l��@l�j@l�@l�D@lj@lZ@l(�@l�@k��@k�m@k�F@kdZ@kS�@k33@ko@jn�@i�^@i�@ihs@i%@h�`@h1'@h �@h  @g+@g�@f��@f�y@fv�@f5?@dj@c�m@cC�@b��@b=q@bn�@b~�@b~�@b�\@b�!@b�!@b��@b^5@b�@a��@a��@a��@a��@aG�@aG�@b��@co@co@b�\@b�\@b~�@bM�@a��@a�@`��@_�P@^��@^�@^��@]�@]��@]p�@]?}@\�@\�D@\�D@\�@\��@\j@\(�@[��@[t�@Y�7@Y��@ZM�@Y��@X��@XbN@Xb@Xb@W�@W�w@V��@VV@U�T@U`B@T�@T�@Sƨ@St�@R��@Qx�@PĜ@P�u@Pr�@P1'@O��@O|�@O;d@N�@Nv�@M�h@L��@L��@L(�@K��@K@J��@J~�@J=q@J��@K@J�H@J��@J��@Jn�@I��@I�@H��@HA�@G�;@G�@G\)@FE�@E�@E`B@E?}@E/@D�@D1@C��@C�
@CC�@C"�@C"�@C"�@C"�@Co@B�@B�!@B�!@B~�@Bn�@B^5@BJ@A��@Ax�@@��@@��@@�u@@r�@@b@?|�@?\)@?K�@?;d@?
=@?
=@>��@>�@>�R@>$�@>@=��@=�@=/@<��@<j@<(�@;�
@;t�@;dZ@;C�@;@;o@:��@9�#@9�^@9��@9X@8��@8�`@8��@81'@7��@7l�@7l�@7l�@7;d@6�R@6v�@6V@65?@6{@5�T@5O�@4�/@4�@4j@4(�@3�m@3�F@3�@2�@2�!@2n�@2=q@2-@1��@1�^@1X@1�@0��@0b@0  @/�@/�@/��@/�w@/��@/|�@/;d@.��@.��@.�+@.V@.{@-�-@-�h@-?}@,�@,�@,j@,9X@,(�@,�@+��@+ƨ@+t�@+dZ@+S�@+S�@+33@*�@*n�@)�#@)x�@)hs@)hs@)hs@)X@)�@(��@(Ĝ@(��@(�@(1'@'��@'|�@';d@&�y@&ȴ@&��@&E�@%�@%�@%/@%�@$�/@$��@$(�@#��@#33@"�H@"��@"��@"��@"�!@"��@"~�@"=q@!�^@!�@ ��@ Ĝ@ �u@ �@  �@   @   @�@�;@�w@l�@;d@��@�+@E�@5?@5?@5?@{@�-@O�@?}@��@�@(�@�m@��@t�@S�@C�@o@@�H@~�@�@�#@X@�9@�u@bN@ �@�@l�@\)@K�@�@��@v�@{@�T@�-@�h@�@��@�D@I�@1@�m@ƨ@��@��@t�@S�@o@��@M�@J@��@�#@��@�@��@Ĝ@Ĝ@�9@��@��@�u@�@1'@b@�;@��@�@��@|�@K�@;d@�y@��@ff@E�@5?@@��@O�@V@��@��@j@1@�
@�F@�@dZ@S�@C�@33@"�@o@
��@
�!@
�\@
~�@
n�@
^5@
=q@
�@	��@	�#@	��@	�^@	��@	�7@	x�@	x�@	x�@	X@	G�@��@r�@Q�@1'@ �@b@  @�;@��@�P@�P@|�@�P@�P@�P@l�@K�@
=@v�@$�@�@@�@O�@V@�@�j@z�@I�@��@�
@ƨ@�F@�@S�@33@"�@@�H@��@�\@�\@~�@n�@n�@^5@M�@-@�@��@hs@%@ ��@ ��@ Ĝ@ �@ A�@ 1'@  �1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BhBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBhBbB\BVBPB
=BB�B�jBT�B=qBuB1B�BƨB�5B��BȴB�-B��B��B��B��B�PBn�Be`Bp�Bs�Bq�Bm�BffBhsBe`B`BBXBQ�BE�B:^B+B�BJBB
��B
��B
��B
�B
�B
ȴB
�^B
�qB
�^B
�RB
�?B
�9B
�!B
�B
��B
��B
��B
��B
�uB
�PB
�=B
�B
~�B
z�B
q�B
hsB
XB
F�B
7LB
9XB
2-B
(�B
 �B
�B
uB
bB
VB
DB
B	��B	��B	��B	�B	�B	�mB	�5B	�#B	�B	�B	��B	��B	��B	��B	�jB	�LB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�PB	�B	|�B	t�B	o�B	k�B	e`B	cTB	dZB	bNB	`BB	ZB	S�B	L�B	E�B	A�B	>wB	@�B	;dB	2-B	/B	,B	-B	+B	'�B	"�B	�B	�B	�B	{B	\B	JB	%B	+B	%B	B	B	B��B��B��B�B�B�B�B�B�B�fB�ZB�NB�BB�)B��B��B��B��B��BȴBĜB��B�jB�^B�'B�B�3B�?B�FB�?B�3B�-B�!B�B�B�B�B�B��B��B��B��B�uB��B��B��B�uB�bB�JB�PB�=B�+B~�B~�Bz�B{�B{�Bt�Bs�Bv�Bv�Bu�Bq�BiyBYBaHBZBYBO�BH�BH�BF�BG�BC�B>wBD�B>wBA�BC�B?}B5?B49B2-B7LB7LB7LB49B:^B;dB6FB6FB7LB5?B49B1'B1'B.B.B1'B7LB;dB:^B8RB5?B.B2-B-B%�B-B0!B/B1'B33B33B6FB6FB7LB6FB6FB5?B1'B,B&�B0!B,B(�B6FB8RB:^B;dB;dB<jB<jB;dB9XB7LB6FB;dB=qB>wB;dB=qB?}BD�BE�BD�BD�BF�BG�BG�BF�B@�B9XB7LBH�BE�BG�BG�BG�BG�BT�BW
BW
BQ�BYB`BBbNBbNBcTBcTBaHBaHB_;B^5Bo�By�B}�B�B~�B�B~�Bx�B|�B�B� B}�B�B�B�B� B{�B{�B� B�B�B�JB�uB��B��B��B��B��B��B��B�B�B�B�'B�?B�RB�^B�dB�dB�jB�jB�qB��BǮBɺB��B��B��B��B��B�B�B�)B�;B�`B�`B�ZB�NB�NB�ZB�yB�yB�fB�mB��B��B��B��B��B��B��B��B��B��B��B	B	1B	
=B	DB	PB	VB	\B	bB	\B	JB	bB	{B	{B	�B	�B	�B	�B	#�B	$�B	$�B	%�B	(�B	)�B	+B	+B	/B	33B	7LB	8RB	<jB	>wB	B�B	B�B	B�B	G�B	J�B	K�B	N�B	P�B	P�B	P�B	Q�B	R�B	T�B	W
B	XB	ZB	[#B	\)B	\)B	\)B	_;B	_;B	aHB	`BB	aHB	aHB	bNB	bNB	bNB	bNB	cTB	ffB	m�B	o�B	q�B	s�B	s�B	s�B	s�B	u�B	v�B	w�B	w�B	y�B	}�B	}�B	}�B	}�B	�B	�%B	�%B	�+B	�1B	�+B	�7B	�PB	�VB	�VB	�\B	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�LB	�RB	�XB	�XB	�XB	�RB	�XB	�LB	�RB	�XB	�jB	��B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	��B	��B	�#B	�/B	�;B	�BB	�HB	�HB	�BB	�BB	�BB	�;B	�BB	�HB	�NB	�HB	�HB	�HB	�HB	�HB	�NB	�TB	�ZB	�fB	�mB	�mB	�sB	�sB	�`B	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
%B
%B
%B
%B
B
B
B
B
B
B
B
B
+B
1B
1B
+B
%B
1B
	7B
	7B

=B
DB
DB
DB
DB
DB
DB
JB
JB
JB
JB
JB
JB
VB
VB
\B
\B
bB
bB
hB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
"�B
#�B
#�B
"�B
!�B
"�B
#�B
#�B
#�B
#�B
"�B
#�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
)�B
(�B
(�B
(�B
)�B
)�B
)�B
-B
-B
-B
-B
-B
-B
-B
-B
.B
/B
0!B
0!B
1'B
1'B
33B
33B
33B
49B
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
7LB
6FB
6FB
7LB
9XB
;dB
;dB
;dB
;dB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
;dB
<jB
=qB
=qB
<jB
<jB
;dB
;dB
<jB
=qB
>wB
?}B
>wB
>wB
>wB
>wB
>wB
=qB
?}B
A�B
A�B
A�B
B�B
A�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
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
XB
XB
W
B
W
B
XB
YB
YB
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
[#B
[#B
[#B
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
bNB
bNB
dZB
dZB
e`B
e`B
e`B
dZB
dZB
ffB
ffB
ffB
ffB
ffB
ffB
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
gmB
hsB
gmB
gmB
hsB
hsB
iyB
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
jB
jB
jB
jB
k�B
k�B
l�B
m�B
l�B
l�B
m�B
n�B
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BhBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoBoB�B}BvBpB�B
�B�B�9B�+BaBE�BBB�B��B�BՁB�)B�zB��B�RB�HB��B��Bt�Bh�Bq[BtBr-Bn}Bg�Bi*Bf2BabBYBS[BG�B=B-�B;B\B?B
��B
��B
��B
�B
�]B
�B
�B
�wB
�0B
��B
��B
��B
��B
��B
��B
��B
�vB
��B
�{B
�pB
��B
�B
�B
{�B
sB
jKB
Z�B
I�B
9�B
:xB
3�B
*�B
"�B
�B
�B
B
�B
�B
�B	�qB	��B	�zB	�nB	�iB	��B	ߤB	��B	ںB	��B	ҽB	уB	�0B	�uB	��B	��B	�5B	��B	��B	��B	�~B	��B	��B	��B	��B	�,B	��B	��B	~�B	v�B	qB	l�B	f�B	dB	d�B	b�B	`�B	[	B	U2B	NVB	F�B	CGB	?HB	AB	<6B	4B	0UB	,�B	-�B	+�B	(�B	$B	B	�B	mB	MB	}B	PB	zB	�B	�B	�B	gB	�B�}B��B�B�}B�B��B��B��B�"B�mB�FB�B��B�/B�mB��B��B��B˒B�lBňB�uB��B�dB�B��B��B��B�zB��B��B��B��B��B�}B�wB��B�kB��B��B�B�7B�gB�B�B�
B�FB�hB�PB��B��B�B��B�B|�B|�B|�Bv`Bt�Bw2Bw2BvFBr�BkkB\)BbNB\BZ�BRTBJ�BJ=BG�BH�BD�B@BESB?�BBBC�B@4B6�B5�B3�B8B88B8lB5�B;JB<6B7�B7LB8B5�B4�B1�B1�B/OB.�B1�B7�B;�B:�B9	B6zB/�B3B.�B($B-�B1B0oB2-B4B3�B6zB6�B7�B6�B6�B5�B1�B-wB(�B1B-�B+B6�B8�B:�B;�B;�B<�B<�B;�B9�B8B7LB;�B>(B>�B<6B=�B?�BD�BF%BESBEmBG+BHBHBG+BA�B;�B9�BI�BF�BH�BH�BH�BIBU2BWsBW�BS�BZB`�Bb�BbhBcnBcnBa�Ba�B_�B_pBo�Bz*B~�B��B�4B�oB�By�B}�B�UB�iB~�B�AB�AB�UB�iB|�B|�B��B��B��B�JB�[B��B��B��B�B�~B�ZB�*B�)B�/B��B�vB�tB��B�xB�B�B��B��B��B��BǮB�	B�.B�&B�B�B�2B�SB�_B�]B�VB�FB�zB�B��B� B�`B��B��B�B��B�TB��B�*B�JB�<B��B�>B�^B�xB�<B�]B	uB	fB	
rB	xB	jB	�B	�B	�B	�B	B	�B	�B	�B	�B	�B	�B	/B	#�B	%B	%B	&2B	)*B	*0B	+6B	+QB	/iB	3MB	7�B	8�B	<�B	>�B	B�B	B�B	CB	G�B	J�B	K�B	N�B	Q B	Q B	QB	R B	S&B	U2B	W?B	XEB	ZQB	[=B	\CB	\]B	\xB	_VB	_pB	aHB	`\B	abB	abB	bhB	bhB	bhB	bhB	cnB	ffB	mwB	o�B	q�B	s�B	s�B	s�B	s�B	u�B	v�B	w�B	xB	z*B	~(B	~B	~(B	~wB	�9B	�YB	�%B	�EB	�KB	�zB	��B	�jB	�pB	�pB	�vB	�bB	�TB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�&B	�B	��B	�B	�$B	�B	�KB	�/B	�LB	��B	�rB	�rB	�rB	��B	�rB	��B	��B	��B	��B	��B	ªB	ÖB	ÖB	ĜB	ĜB	ŢB	żB	żB	żB	��B	ǮB	��B	��B	��B	ˬB	ѝB	�#B	�IB	�pB	�BB	�|B	�bB	��B	�vB	�vB	ߤB	�vB	�bB	�hB	�|B	�bB	�bB	�bB	�|B	�hB	�TB	�ZB	�B	�B	�B	�B	��B	��B	�fB	�sB	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�*B	��B	�B	�"B	�.B
 B
 B
 B
�B
B
9B
?B
?B
?B
YB
mB
9B
MB
gB
3B
MB
uB
MB
EB
fB
1B
_B
tB
KB
	RB
	lB

XB
DB
DB
DB
^B
^B
xB
JB
~B
dB
dB
dB
dB
pB
�B
�B
vB
}B
�B
�B
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
!�B
"�B
#�B
#�B
"�B
!�B
"�B
#�B
#�B
#�B
#�B
#B
#�B
%�B
%�B
%�B
'B
'B
&�B
'8B
(
B
)B
)B
)�B
)B
)B
)*B
)�B
*0B
*0B
-B
-B
-B
-)B
-B
-)B
-)B
-)B
./B
/5B
0;B
0UB
1AB
1AB
33B
3MB
3MB
4TB
5ZB
5ZB
6FB
6FB
6`B
6`B
6`B
7LB
7LB
7LB
7fB
6zB
6zB
7�B
9rB
;dB
;B
;dB
;B
:xB
:xB
:^B
:�B
:xB
:�B
:�B
;�B
;B
;B
<jB
<�B
<�B
;�B
<�B
=VB
=qB
<�B
<�B
;�B
;�B
<�B
=�B
>wB
?}B
>wB
>�B
>�B
>�B
>�B
=�B
?�B
A�B
A�B
A�B
B�B
A�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
F�B
F�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
MB
N�B
N�B
N�B
OB
O�B
P�B
Q B
QB
QB
QB
RB
SB
SB
SB
S&B
TB
S�B
TB
S�B
UB
UB
VB
VB
VB
VB
VB
V9B
W
B
W$B
XB
X+B
W$B
WYB
X+B
YB
YB
YB
YB
YB
YB
Y1B
Y1B
Y1B
Z7B
ZB
Z7B
ZB
Z7B
[=B
[=B
[=B
\]B
\CB
]IB
]IB
]/B
]/B
]IB
^OB
^5B
^OB
_VB
_VB
`\B
`\B
`\B
`\B
aHB
aHB
abB
abB
abB
abB
abB
abB
bNB
bNB
bhB
bhB
bhB
bhB
bhB
bNB
bhB
cTB
cnB
cTB
cTB
cTB
cTB
cnB
bNB
b�B
dZB
d�B
e`B
e`B
e`B
dZB
dtB
ffB
ffB
ffB
f�B
ffB
fLB
ezB
ezB
ezB
e�B
f�B
f�B
g�B
g�B
g�B
g�B
h�B
g�B
g�B
h�B
h�B
iyB
iyB
i�B
iyB
iyB
iyB
jB
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
j�B
j�B
j�B
j�B
k�B
k�B
l�B
m�B
l�B
l�B
m�B
n�B
n�B
n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,1<AT�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201711250034182017112500341820171125003418201806221233482018062212334820180622123348201804050429482018040504294820180405042948  JA  ARFMdecpA19c                                                                20171121093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20171121003523  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20171121003525  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20171121003525  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20171121003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20171121003526  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20171121003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20171121003526  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20171121003526  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20171121003527                      G�O�G�O�G�O�                JA  ARUP                                                                        20171121005515                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20171121153601  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20171124153418  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20171124153418  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404192948  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033348  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121516                      G�O�G�O�G�O�                