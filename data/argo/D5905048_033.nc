CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-31T00:35:18Z creation;2016-08-31T00:35:20Z conversion to V3.1;2019-12-19T08:27:54Z update;     
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20160831003518  20200116201517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               !A   JA  I2_0577_033                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��5z�1   @��68� @4$%��1��d��O�M1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� DfD�fD  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� DfD�fD  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A�ĜA�wA�wA�wA�wA�wA�A�A�A�jA�wA�wA���A�A�A���A���A���A�A⛦A���A��HA��A���A�hA�%A�33A�hsA�&�A���A�M�Aݗ�A�7LAܺ^A�jA��AۋDA�$�A�$�Aى7A�?}A�JA�;dA�oAӶFA�O�A��A���A�jA�A�?}A��`A�A�A���A�+A�A���A�M�Ać+AÓuA��/A£�A�S�A��\A��A��A��7A�O�A��A�oA�bA���A�x�A���A��9A�v�A��A���A��
A��A��wA�/A��A��-A���A��jA�(�A���A�O�A��A�E�A��A���A��hA�z�A�O�A���A�jA���A�A�A���A���A��wA��hA���A�=qA�XA���A�C�A��!A���A�?}A��HA�%A���A��A�S�A��A�bNA�^5A�?}A�/A��DA�;dA?}A~$�A|��Ax�!AvbAshsAo\)Ann�Am33Ak��Aj~�AhA�Af�Ae33Ad��Ac��AbM�Aa�A]�-A[�A[?}AZ=qAYC�AW�PAVZAU��AS��AQ�AO\)AL�AJ��AJ�\AIoAG�AD��AB^5A@r�A?|�A<ĜA;�^A:��A9�^A8Q�A7�mA7��A7|�A7G�A6�RA5hsA2$�A0��A0�A0=qA/�A.�RA.=qA,��A*�A)�A(��A&��A%��A#�FA"^5A �AjA��A��AAC�A �A�RA�HA�PA5?A�At�Ar�A�RA��A�A  A
�RA	�#A�
A�AVAVA�AA�A`BA�yA�AA�A�PA �yA ��A �\@���@�p�@�Z@��y@�b@�l�@���@�1'@���@�bN@���@�\)@�o@�z�@�@旍@�h@�O�@�j@���@�\)@�ff@��@�z�@���@�@ܣ�@ۅ@ڰ!@�M�@�@�X@�V@ؓu@�I�@�^5@��@�
=@��H@�`B@ؓu@�9X@�b@��;@պ^@ӝ�@�@�/@Л�@�b@��@���@�1@�ƨ@�"�@Ώ\@�-@�@͡�@��@�j@���@�|�@�;d@�"�@���@ʸR@�n�@�5?@��/@�Q�@��@��@ǝ�@�S�@��@őh@�hs@�V@�1'@�ƨ@�\)@�@���@�p�@�p�@�p�@�`B@�`B@�O�@���@��P@�;d@�=q@���@��@� �@���@�33@��@��!@���@��h@��@�X@��u@��w@�;d@�v�@��-@�/@�%@�M�@�t�@��
@�|�@��u@��@��@�"�@�`B@�+@�^5@���@�G�@���@�I�@���@��F@���@��@��@�E�@�{@��T@��@�X@�z�@��P@�|�@��H@��+@�^5@�=q@�@��-@���@��#@�p�@��@�z�@�I�@��m@��@�M�@��#@��7@�`B@�V@�/@�?}@�/@��/@��@��@���@��w@���@��@���@��m@�;d@���@�E�@�5?@���@�^5@�V@�V@�-@�M�@�E�@�ff@�ff@�V@�n�@�E�@��@��@�hs@�O�@�&�@��@���@�z�@�I�@�9X@�1'@�1@��@��;@���@���@�|�@�"�@��H@��!@�v�@�ff@�M�@�=q@�5?@�@���@��^@��h@�?}@���@�z�@�j@�Z@�I�@�A�@�  @��F@��@�K�@��@��y@��R@�n�@�@���@���@��@���@��-@�x�@�&�@��9@���@��@�I�@��
@���@�t�@�dZ@��H@�ȴ@���@�~�@���@���@�`B@�/@�V@���@���@��@�j@�A�@�1@��;@�ƨ@�|�@�33@��@�ff@�=q@��@�hs@�O�@���@���@�z�@�r�@�1'@��@���@��P@�|�@�o@�@�X@�&�@��@�9X@��@�t�@�K�@�+@�ȴ@���@���@���@�-@��@��T@���@��h@�7L@���@��9@�Z@��m@��w@���@�l�@���@��R@�~�@��T@��-@��h@�G�@�/@�V@��@��j@��@�@~��@~��@}��@|�@|�D@|(�@{�F@{"�@z�H@z�!@z�!@z��@z~�@z~�@z-@y��@y��@yhs@y7L@y%@x��@xĜ@xA�@w�@wl�@w�@w
=@w
=@v��@vE�@uO�@t�@tZ@tI�@t9X@s�m@s�@s@r�!@rJ@qG�@p�@pQ�@p1'@o�@o;d@nȴ@n�+@nE�@m�@m�T@m�T@m�T@m��@m��@m��@m�h@m��@m�@mp�@m�@k�m@k��@k33@ko@j�@j��@j�\@j�\@j~�@j^5@jM�@jJ@i��@ix�@iG�@h��@hr�@hr�@hQ�@h  @g�P@g;d@f��@f�+@f5?@f@e�T@e��@eO�@d��@dj@d(�@d�@c��@b�@b~�@b-@bJ@a��@a�7@a7L@`��@`bN@` �@_��@_;d@^�@^�+@^5?@]�@]��@]?}@]�@\�@[�m@[dZ@Z��@Z^5@ZJ@Y��@Y�^@Y��@Y��@Yx�@Y%@X��@Xr�@X1'@X  @W�w@W�P@V��@VE�@U@U/@T��@T��@T�D@TZ@T9X@T1@S�
@S�@SdZ@S33@R�@Rn�@Q��@Q��@QX@P�`@P��@P�9@P�u@P�u@P�@PbN@P1'@O�;@O;d@Nȴ@Nff@NE�@N5?@M�@M�-@M�h@M?}@MV@L��@L9X@L1@K�F@K@Jn�@J�@I��@I�#@I��@I��@Ix�@Ihs@IX@I&�@H�9@Hb@G�@Gl�@G;d@F�y@F��@Fv�@FE�@E�T@E�@Ep�@E`B@E�@D�@D��@D9X@C�m@Cƨ@Ct�@CC�@C@B��@B�H@B�H@B�\@BM�@A�@Ahs@A&�@@�`@@�u@@b@?
=@>@=��@=�@=�@<�@<�j@<�D@<j@;ƨ@:�H@:��@:��@:~�@:J@9hs@9&�@8��@8bN@8bN@8Q�@7�;@7��@7l�@7+@7
=@6ȴ@6v�@6$�@5�-@5?}@4�/@4�D@4I�@3��@3S�@333@3"�@2�@2�\@1��@1hs@1X@1X@1G�@1%@0�9@0bN@0  @/l�@.��@.�R@.��@.�+@.E�@.5?@.{@-@-O�@,��@,��@,��@,�D@,9X@,�@+ƨ@+ƨ@+��@+��@+C�@*�@*��@*n�@*�@)�#@)��@)��@)X@)G�@(��@(Ĝ@(�9@(��@(�@(Q�@(1'@(b@'�@'�w@'|�@'\)@'�@&ȴ@&�+@&5?@%�T@%��@%O�@%/@$��@$�@$z�@$(�@#�
@#�@#dZ@#"�@"�H@"��@"��@"�@!�#@!�#@!��@!�^@!hs@!&�@ �@�@��@�P@|�@K�@��@�R@��@v�@$�@V@V@@�h@�h@p�@O�@/@�@V@�@�/@�/@�@I�@(�@�@�
@��@��@�@�@S�@"�@�H@��@�\@�\@~�@n�@M�@=q@=q@�@�@X@��@�u@Q�@1'@  @�@�P@l�@;d@+@�@�@
=@�y@V@@@@�@��@��@�@O�@�@�@��@�D@Z@I�@9X@9X@�@1@�m@ƨ@��@��@��@t�@dZ@C�@"�@o@�@��@��@M�@J@��@�7@X@%@Ĝ@�u@r�@A�@A�@1'@  @�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A�ĜA�wA�wA�wA�wA�wA�A�A�A�jA�wA�wA���A�A�A���A���A���A�A⛦A���A��HA��A���A�hA�%A�33A�hsA�&�A���A�M�Aݗ�A�7LAܺ^A�jA��AۋDA�$�A�$�Aى7A�?}A�JA�;dA�oAӶFA�O�A��A���A�jA�A�?}A��`A�A�A���A�+A�A���A�M�Ać+AÓuA��/A£�A�S�A��\A��A��A��7A�O�A��A�oA�bA���A�x�A���A��9A�v�A��A���A��
A��A��wA�/A��A��-A���A��jA�(�A���A�O�A��A�E�A��A���A��hA�z�A�O�A���A�jA���A�A�A���A���A��wA��hA���A�=qA�XA���A�C�A��!A���A�?}A��HA�%A���A��A�S�A��A�bNA�^5A�?}A�/A��DA�;dA?}A~$�A|��Ax�!AvbAshsAo\)Ann�Am33Ak��Aj~�AhA�Af�Ae33Ad��Ac��AbM�Aa�A]�-A[�A[?}AZ=qAYC�AW�PAVZAU��AS��AQ�AO\)AL�AJ��AJ�\AIoAG�AD��AB^5A@r�A?|�A<ĜA;�^A:��A9�^A8Q�A7�mA7��A7|�A7G�A6�RA5hsA2$�A0��A0�A0=qA/�A.�RA.=qA,��A*�A)�A(��A&��A%��A#�FA"^5A �AjA��A��AAC�A �A�RA�HA�PA5?A�At�Ar�A�RA��A�A  A
�RA	�#A�
A�AVAVA�AA�A`BA�yA�AA�A�PA �yA ��A �\@���@�p�@�Z@��y@�b@�l�@���@�1'@���@�bN@���@�\)@�o@�z�@�@旍@�h@�O�@�j@���@�\)@�ff@��@�z�@���@�@ܣ�@ۅ@ڰ!@�M�@�@�X@�V@ؓu@�I�@�^5@��@�
=@��H@�`B@ؓu@�9X@�b@��;@պ^@ӝ�@�@�/@Л�@�b@��@���@�1@�ƨ@�"�@Ώ\@�-@�@͡�@��@�j@���@�|�@�;d@�"�@���@ʸR@�n�@�5?@��/@�Q�@��@��@ǝ�@�S�@��@őh@�hs@�V@�1'@�ƨ@�\)@�@���@�p�@�p�@�p�@�`B@�`B@�O�@���@��P@�;d@�=q@���@��@� �@���@�33@��@��!@���@��h@��@�X@��u@��w@�;d@�v�@��-@�/@�%@�M�@�t�@��
@�|�@��u@��@��@�"�@�`B@�+@�^5@���@�G�@���@�I�@���@��F@���@��@��@�E�@�{@��T@��@�X@�z�@��P@�|�@��H@��+@�^5@�=q@�@��-@���@��#@�p�@��@�z�@�I�@��m@��@�M�@��#@��7@�`B@�V@�/@�?}@�/@��/@��@��@���@��w@���@��@���@��m@�;d@���@�E�@�5?@���@�^5@�V@�V@�-@�M�@�E�@�ff@�ff@�V@�n�@�E�@��@��@�hs@�O�@�&�@��@���@�z�@�I�@�9X@�1'@�1@��@��;@���@���@�|�@�"�@��H@��!@�v�@�ff@�M�@�=q@�5?@�@���@��^@��h@�?}@���@�z�@�j@�Z@�I�@�A�@�  @��F@��@�K�@��@��y@��R@�n�@�@���@���@��@���@��-@�x�@�&�@��9@���@��@�I�@��
@���@�t�@�dZ@��H@�ȴ@���@�~�@���@���@�`B@�/@�V@���@���@��@�j@�A�@�1@��;@�ƨ@�|�@�33@��@�ff@�=q@��@�hs@�O�@���@���@�z�@�r�@�1'@��@���@��P@�|�@�o@�@�X@�&�@��@�9X@��@�t�@�K�@�+@�ȴ@���@���@���@�-@��@��T@���@��h@�7L@���@��9@�Z@��m@��w@���@�l�@���@��R@�~�@��T@��-@��h@�G�@�/@�V@��@��j@��@�@~��@~��@}��@|�@|�D@|(�@{�F@{"�@z�H@z�!@z�!@z��@z~�@z~�@z-@y��@y��@yhs@y7L@y%@x��@xĜ@xA�@w�@wl�@w�@w
=@w
=@v��@vE�@uO�@t�@tZ@tI�@t9X@s�m@s�@s@r�!@rJ@qG�@p�@pQ�@p1'@o�@o;d@nȴ@n�+@nE�@m�@m�T@m�T@m�T@m��@m��@m��@m�h@m��@m�@mp�@m�@k�m@k��@k33@ko@j�@j��@j�\@j�\@j~�@j^5@jM�@jJ@i��@ix�@iG�@h��@hr�@hr�@hQ�@h  @g�P@g;d@f��@f�+@f5?@f@e�T@e��@eO�@d��@dj@d(�@d�@c��@b�@b~�@b-@bJ@a��@a�7@a7L@`��@`bN@` �@_��@_;d@^�@^�+@^5?@]�@]��@]?}@]�@\�@[�m@[dZ@Z��@Z^5@ZJ@Y��@Y�^@Y��@Y��@Yx�@Y%@X��@Xr�@X1'@X  @W�w@W�P@V��@VE�@U@U/@T��@T��@T�D@TZ@T9X@T1@S�
@S�@SdZ@S33@R�@Rn�@Q��@Q��@QX@P�`@P��@P�9@P�u@P�u@P�@PbN@P1'@O�;@O;d@Nȴ@Nff@NE�@N5?@M�@M�-@M�h@M?}@MV@L��@L9X@L1@K�F@K@Jn�@J�@I��@I�#@I��@I��@Ix�@Ihs@IX@I&�@H�9@Hb@G�@Gl�@G;d@F�y@F��@Fv�@FE�@E�T@E�@Ep�@E`B@E�@D�@D��@D9X@C�m@Cƨ@Ct�@CC�@C@B��@B�H@B�H@B�\@BM�@A�@Ahs@A&�@@�`@@�u@@b@?
=@>@=��@=�@=�@<�@<�j@<�D@<j@;ƨ@:�H@:��@:��@:~�@:J@9hs@9&�@8��@8bN@8bN@8Q�@7�;@7��@7l�@7+@7
=@6ȴ@6v�@6$�@5�-@5?}@4�/@4�D@4I�@3��@3S�@333@3"�@2�@2�\@1��@1hs@1X@1X@1G�@1%@0�9@0bN@0  @/l�@.��@.�R@.��@.�+@.E�@.5?@.{@-@-O�@,��@,��@,��@,�D@,9X@,�@+ƨ@+ƨ@+��@+��@+C�@*�@*��@*n�@*�@)�#@)��@)��@)X@)G�@(��@(Ĝ@(�9@(��@(�@(Q�@(1'@(b@'�@'�w@'|�@'\)@'�@&ȴ@&�+@&5?@%�T@%��@%O�@%/@$��@$�@$z�@$(�@#�
@#�@#dZ@#"�@"�H@"��@"��@"�@!�#@!�#@!��@!�^@!hs@!&�@ �@�@��@�P@|�@K�@��@�R@��@v�@$�@V@V@@�h@�h@p�@O�@/@�@V@�@�/@�/@�@I�@(�@�@�
@��@��@�@�@S�@"�@�H@��@�\@�\@~�@n�@M�@=q@=q@�@�@X@��@�u@Q�@1'@  @�@�P@l�@;d@+@�@�@
=@�y@V@@@@�@��@��@�@O�@�@�@��@�D@Z@I�@9X@9X@�@1@�m@ƨ@��@��@��@t�@dZ@C�@"�@o@�@��@��@M�@J@��@�7@X@%@Ĝ@�u@r�@A�@A�@1'@  @�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�FB
�FB
�FB
�FB
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�?B
�LB
�XB
�jB
�qB
��B
�BB1B%BBBB  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��BB
��B
��B
��BB%BDBJBPBVB�B�B)�B0!B8RBYBO�Bo�B��B�;B��B��B5?Bo�B}�BO�BH�B=qBQ�BiyBy�B�VB��B�XB�mBJB�B�B�B �B�BoB1BB��B��B�B�yB�NB�HB�
B��BÖB�}B�jB�LB�B��B~�Bv�Bq�BgmBVB+BB��B}�Bm�Bl�Bm�Bu�Bu�Bp�Be`B[#BH�B0!B�BhB+B
��B
�B
�B
�oB
�%B
�7B
��B
�VB
�B
w�B
^5B
E�B
6FB
�B
oB
DB
  B	��B	�B	�B	��B	��B	ŢB	�jB	�?B	��B	�uB	�VB	�7B	�B	z�B	q�B	m�B	bNB	W
B	C�B	7LB	,B	'�B	"�B	�B	
=B	B��B�B�sB�TB�BB�)B�
B�B��B��B��B��B��B��B�jB�^B�RB�LB�3B�!B�B��B��B��B��B��B�uB�\B�JB�%B�B�B�B~�B|�By�Bu�Br�Bp�Bo�Bl�Bk�Bk�BhsBiyBn�BjBhsBffBhsBhsBhsBgmBe`BdZBbNBbNB_;B_;BaHB`BBaHBdZBe`BcTBaHBbNBbNBcTBgmBiyBiyBiyBiyBhsBl�Bm�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bo�Bq�Bs�Bt�Bu�Bx�Bz�B}�B�+B�oB��B��B��B�jBĜBɺB��B��B�B�#B�#B�B�ZB�fB�mB�sB�B�B�B�B��B��B��B��B��B	B	B	B	%B	1B		7B	
=B	
=B	
=B	DB	JB	PB	oB	{B	�B	�B	�B	�B	�B	 �B	!�B	"�B	&�B	&�B	(�B	,B	/B	0!B	0!B	0!B	0!B	0!B	/B	2-B	49B	6FB	=qB	?}B	B�B	G�B	J�B	L�B	L�B	M�B	P�B	T�B	XB	ZB	ZB	\)B	aHB	bNB	`BB	bNB	dZB	q�B	}�B	�B	�%B	� B	{�B	�B	�%B	�B	�B	� B	� B	�B	�B	�=B	�JB	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�LB	�FB	�?B	�FB	�XB	�XB	�RB	�RB	�dB	�dB	�qB	B	ĜB	ĜB	ŢB	ǮB	ɺB	��B	��B	ǮB	��B	��B	��B	�B	��B	��B	�B	�)B	�5B	�5B	�BB	�HB	�TB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
+B
1B
1B
	7B
DB
DB
JB
JB
JB
JB
PB
VB
PB
VB
PB
VB
VB
VB
bB
bB
bB
bB
bB
bB
bB
bB
bB
\B
PB
JB
DB

=B
	7B
	7B
	7B

=B
DB
JB
PB
PB
PB
\B
bB
bB
hB
hB
hB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
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
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
.B
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
1'B
1'B
1'B
1'B
2-B
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
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
5?B
5?B
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
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
I�B
I�B
J�B
I�B
I�B
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
M�B
M�B
M�B
M�B
M�B
M�B
O�B
N�B
O�B
O�B
O�B
O�B
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
VB
VB
VB
W
B
W
B
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
[#B
[#B
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
]/B
]/B
]/B
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
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
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
e`B
e`B
e`B
e`B
ffB
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
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
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
k�B
l�B
l�B
l�B
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�FB
�FB
�`B
�FB
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�FB
�FB
�FB
�FB
�ZB
�LB
�XB
�jB
��B
�UB
�HBfBYBmB�BgB�B
�JB
��B
��B
�B
�0B
��B
��B
��B
��B
��B
��B�B OB
�BB
��B�B	�BB6BpB.B�B �B,B33B;�B[�BR:Br�B��B�B�wB�6B6BshB��BU�BL�B@4BR�Bi�BzDB�pB�5B��B�_BBIB �B!bB#:B�BB
�B�B'B�B��B�B�ZB�tBٚB�bB�MB�OB�B�DB�B��B�Bx8BsBi�BY�B0�BdB�1B�;Bo�Bn}Bn�Bw2Bw�BshBh�B^�BL�B2�B �B@B	�B�B
�B
��B
��B
��B
�XB
�B
�B
��B
|B
a�B
I7B
:*B
5B
,B
6B
�B	�B	�B	�QB	��B	�6B	�zB	��B	��B	��B	��B	��B	��B	�B	|jB	r�B	p!B	eB	ZkB	GB	8�B	-B	*0B	%zB	�B	6B	aB��B�B��B�B��BݲBרB�mB�gBԕB�,B� B�(B��B�"B��B�>B��B�TB�B�oB��B��B��B��B�CB��B��B��B�EB��B�GB�B��B~�B|6Bw�Bt�BraBq�Bn/Bm�Bl�Bi�Bj�BpUBl"Bj�Bg8Bh�Bh�Bh�BhsBf�BeFBc�BdZB`BB`Ba�B`�Bb�Be�BfLBd�Bb�BcBc:BeBh�BjeBi�Bi�BjKBjBn�Bn�BqABp�Bq'BqABq'Bp;Bp;Bp�Br�Bt�Bu�Bv�By�B{JB~wB��B��B��B��B��B�BĶB�#B��B՛B�eB�qB��B��B�B�mB�
B��B��B��B�B�B�%B�>B�DB�<B�HB	AB	�B	�B	�B	�B		lB	
XB	
rB	
�B	xB	�B	"B	�B	�B	�B	�B	�B	B	B	 �B	"4B	#nB	'8B	'RB	)�B	,�B	/5B	0!B	0;B	0;B	0;B	0UB	/�B	2�B	4�B	6�B	=�B	@B	C-B	HB	KB	MB	MB	N<B	QB	UB	XEB	Z�B	Z�B	\�B	a�B	b�B	`�B	b4B	c�B	qAB	~B	��B	��B	��B	{�B	��B	�_B	�AB	��B	�iB	�OB	�UB	��B	��B	�~B	�jB	��B	��B	��B	��B	��B	�B	�B	�hB	�ZB	�B	�_B	�CB	�"B	�"B	�]B	�"B	�B	�AB	��B	��B	�zB	��B	��B	��B	��B	��B	��B	�B	��B	�qB	B	ĶB	��B	żB	��B	��B	�jB	�)B	�zB	ʌB	��B	�gB	�9B	�gB	��B	��B	�CB	�OB	�OB	�\B	�HB	�:B	�fB	�sB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	�B	�0B	�B	�B	�6B	�VB	�B	�B	�.B	�B	�B	�.B	�.B
 4B
 4B
 4B
 OB
;B
;B
;B
B
B
B
'B
'B
AB
[B
aB
3B
3B
MB
gB
YB
?B
?B
tB
EB
EB
_B
zB
_B
EB
EB
EB
KB
fB
	lB
^B
xB
~B
dB
dB
~B
�B
�B
�B
�B
�B
�B
pB
�B
�B
}B
�B
�B
�B
}B
}B
�B
�B
B
�B
~B
�B

rB
	�B
	�B
	lB

rB
xB
dB
PB
jB
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
�B
�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
#B
"�B
"�B
"�B
"�B
#B
"�B
#B
$B
#�B
#�B
#�B
#�B
#�B
$&B
%,B
%B
&B
%�B
%�B
&B
'B
'B
'B
'8B
'8B
($B
(
B
(
B
($B
)*B
)*B
)B
)B
*B
)�B
)�B
)�B
)�B
)�B
*B
+B
)�B
*B
*B
*B
+kB
,"B
,B
,"B
,"B
,"B
,"B
-B
-)B
.B
./B
./B
./B
./B
./B
./B
/OB
/B
/5B
/5B
/OB
0!B
0;B
0UB
0!B
0;B
1AB
1AB
1[B
1[B
2GB
2GB
2GB
2aB
3hB
3hB
4TB
4TB
49B
4TB
4nB
4nB
5?B
5ZB
5tB
5tB
5tB
5ZB
5ZB
6`B
6`B
6`B
6`B
6`B
6�B
5tB
5tB
6`B
6zB
7fB
7fB
8RB
8lB
8�B
8�B
9rB
9rB
9rB
9rB
9rB
9rB
9�B
:�B
:�B
;B
;B
;B
<�B
<�B
<�B
<�B
=�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>�B
>wB
>wB
>�B
>wB
>�B
>�B
>�B
>�B
?�B
?�B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
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
I�B
I�B
J�B
I�B
I�B
J�B
KB
J�B
K�B
K�B
K�B
LB
L0B
M6B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
N"B
N"B
O�B
N�B
O�B
PB
PB
O�B
QB
Q B
Q B
Q B
QB
Q�B
RB
RB
RB
SB
SB
SB
S&B
S&B
TB
TB
TB
T,B
T�B
UB
UB
UB
U2B
UgB
VB
VB
VB
VB
VB
VB
VB
W?B
W?B
X+B
XEB
Y1B
Y1B
Y1B
Y1B
Y1B
Y1B
YKB
Z7B
Z7B
Z7B
Z7B
Z7B
ZQB
[WB
[#B
[=B
[=B
[=B
[=B
[=B
[=B
[=B
\)B
\CB
\CB
\CB
\CB
\CB
]IB
]/B
]/B
]IB
]IB
]IB
]IB
]IB
]IB
^OB
^OB
^OB
^OB
_VB
_VB
_VB
_VB
`vB
`\B
`\B
`\B
`\B
`\B
abB
abB
abB
aHB
abB
bNB
b�B
b�B
bhB
cTB
bhB
b�B
bhB
cnB
c�B
d�B
dtB
dZB
dtB
dtB
ezB
ezB
e�B
ezB
ezB
ffB
gmB
g�B
g�B
hsB
h�B
h�B
h�B
hsB
h�B
h�B
hsB
hsB
h�B
i�B
i�B
i�B
i�B
i�B
iyB
iyB
iyB
j�B
j�B
j�B
k�B
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
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
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
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609040039542016090400395420160904003954201806221301322018062213013220180622130132201804050700422018040507004220180405070042  JA  ARFMdecpA19c                                                                20160831093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160831003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160831003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160831003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160831003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160831003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160831003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160831003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160831003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160831003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20160831011958                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160831153359  CV  JULD            G�O�G�O�F�9�                JM  ARCAJMQC2.0                                                                 20160903153954  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160903153954  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220042  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040132  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201517                      G�O�G�O�G�O�                