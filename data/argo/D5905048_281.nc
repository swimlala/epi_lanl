CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-14T00:35:24Z creation;2018-09-14T00:35:28Z conversion to V3.1;2019-12-19T07:28:56Z update;     
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20180914003524  20200116231518  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_281                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؁6V�&�1   @؁7W:� @4�U�=��d\�҈�p1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  BffB   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�Ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��hA��A� �A��;A�?}A��
A�?}A��AЮA�I�AΩ�AΏ\A�VA��Aˏ\A��/A�A�K�A���A�|�AāA��;A���A��/A�jA��A���A�jA���A��A�^5A��A���A��-A�K�A�1A��A�~�A�bNA�A��A�`BA��yA�%A�ƨA�ZA�ffA�G�A���A�bA�?}A��jA��hA�=qA��A��7A�O�A�(�A���A�S�A�7LA��!A�jA�`BA�O�A�jA�ƨA�ZA�9XA���A��7A��jA�-A���A��jA���A�+A�{A���A�A�ȴA�l�A��
A�~�A�+A�p�A�=qA�A��A��-A�1A��DA{�Ax�jAw`BAp�`Ah9XAd�AbĜAa�A]ƨAY+AXn�AXA�AU�PAQ�AP��AN �AJ��AH�AHz�AH  AG��AF��AE�AD�AC��AB=qAA33A@I�A?��A>�A<�RA;�#A:�`A9�A6I�A4�A41A3�A2-A1/A/�A-C�A+�A+�hA+l�A*1A( �A&�A%&�A#;dA!��AC�AJAVA�9A��A�wAZA7LA�jA��A=qA�/A��A"�A �A"�A��A1A
�`A	��A	�#A	�;A	`BA  AȴAZA1A�-AA�RAVA�AG�A��Ar�A1'AA��Ap�A�PA�7Ax�AƨAx�A ȴA 1@�ƨ@��@��@��@�%@�V@�j@��w@���@�t�@��@��H@�E�@��#@��^@�7L@���@��9@�Z@��m@��@��#@�hs@�V@�(�@�!@���@��@�r�@��;@�@�u@�"�@���@�D@�9X@�!@�K�@��@�33@�$�@ش9@թ�@ԓu@�K�@�ff@�J@щ7@�t�@Η�@��`@˕�@ʧ�@�@�z�@Ǯ@�\)@�\)@�  @ʇ+@�b@���@��T@��@ͺ^@�x�@̋D@��@��@�1'@�%@�Ĝ@���@ũ�@��@�o@�J@��h@��7@�X@��@��@�x�@�(�@���@��@�V@���@���@���@�7L@�ƨ@��@��-@�%@���@��@�E�@���@�p�@��`@�1@�33@���@�ff@�n�@��@�&�@��@�r�@��@�I�@��F@�K�@��@���@�E�@�-@��!@���@�
=@�l�@��@�ȴ@�J@���@��^@��@��@��@�G�@� �@��P@�ȴ@�~�@�@��^@�hs@�/@���@���@��@�r�@�bN@�A�@� �@��@��@��P@�"�@�@��H@���@�n�@�J@���@��7@�7L@�r�@�9X@��m@�l�@�+@��@�M�@�5?@�E�@�E�@��@���@��7@�&�@���@��u@�1@��;@���@��w@���@�|�@�\)@�"�@��y@�~�@�^5@�J@��#@��^@���@�O�@�&�@��@��/@��D@�  @��w@���@�S�@�33@��@�ȴ@�^5@�$�@��@���@�`B@�7L@�&�@��@�V@��@��9@�(�@�  @���@�K�@�o@���@��+@�E�@�~�@���@���@���@�v�@�V@�5?@�{@��T@���@��h@�O�@�Ĝ@�z�@�Q�@�I�@�9X@�  @��@��@�dZ@�S�@�;d@��H@�ff@�V@�V@�=q@�-@��T@�%@���@���@��u@�Z@���@�dZ@�\)@�\)@�o@��!@�v�@�^5@�M�@�$�@��-@���@��h@��7@�X@�X@�?}@��/@���@�Q�@��;@�33@���@��H@�ȴ@�~�@�-@���@�%@��j@��j@��`@�%@�Ĝ@�A�@��F@�K�@�o@��y@��!@�v�@�M�@�E�@��@���@��7@�G�@��@��@���@��@��/@���@�j@�I�@� �@�1@�  @�1@�b@�b@��@K�@�@
=@~��@~5?@~{@~$�@}`B@|��@|�@}V@|�@|�D@|�@|j@{��@{C�@z�H@z�!@zM�@yhs@x�@x1'@w��@w��@w�P@w|�@wl�@w\)@wK�@v�+@u�@u/@t��@s��@s��@s�@s@r�!@r�\@r~�@r~�@rM�@q��@q��@q��@q&�@p �@ol�@o�@o;d@oK�@o;d@n�y@n5?@m��@m`B@m/@mV@l�@k33@j��@i�@h�@hA�@h  @g��@f�R@fE�@f{@e�@e@e�@d��@dZ@c�
@c�@ct�@cS�@c"�@b��@b^5@b-@a�#@a��@ax�@ahs@ax�@`�`@`��@` �@_�P@_�P@_\)@_+@^v�@]@]��@]�h@]�h@]�@]?}@]/@]�@]V@\��@\�/@\�/@\�j@\Z@\9X@[ƨ@[�F@[�F@[��@[t�@[dZ@Z��@Z^5@Z^5@Z^5@Zn�@Yx�@X��@X�u@Xr�@XQ�@X �@W;d@U�h@U�@T�/@T��@T�j@T�@T(�@T1@S��@SS�@R�@R��@R�!@Rn�@Q��@Q�7@Q&�@P��@P�@P1'@O�P@OK�@N��@Nff@N$�@N@M��@MO�@MV@Lz�@L1@K33@J�@J��@J�!@J^5@I��@I�^@I�7@I7L@HĜ@Hr�@H  @G+@F��@F�@F�R@F��@F5?@E�@E@E�-@E��@E�@D�/@Dj@C�@CdZ@CdZ@CC�@Co@B�H@B��@B^5@A��@Ax�@AX@@Ĝ@@�@@Q�@?�@?��@?�@?�@?�P@?�P@?�P@?l�@?;d@?
=@>�+@>V@=�@=@=@=p�@<�@<�@<z�@<(�@<�@;��@;�m@;t�@;33@;C�@;33@;o@;@;@;@:�@:�@:��@:n�@:-@9��@9��@9hs@9�@9%@8��@8�`@8��@8b@7��@7\)@7�@6�@6��@6�+@6v�@6ff@6E�@65?@6{@5�@5�-@5?}@5V@4�j@4�D@4z�@4Z@4(�@41@3��@3�F@3o@2��@2^5@2�@1��@17L@1�@1�@0��@0��@0Ĝ@0r�@0r�@0r�@0bN@0b@/�@/l�@/;d@/+@/�@/
=@.�R@.@-�T@-��@-V@,�j@,j@,(�@+�
@+�@+dZ@+S�@+"�@*n�@*J@)�7@)7L@)�@(��@(�9@(bN@(  @'�w@'�P@'\)@&�y@&ȴ@&�R@&��@&��@&��@&��@&ff@&E�@&{@%�T@%?}@$��@$�/@$�j@$��@$Z@$1@#��@#t�@#33@"�@"��@"n�@"^5@"M�@"J@!�@!�#@!��@!&�@!%@ �`@ bN@ Q�@ b@��@\)@;d@
=@�@�+@5?@{@�T@�@?}@/@�/@��@z�@j@I�@I�@��@��@t�@C�@�H@�H@�H@��@��@�!@��@~�@^5@^5@-@��@��@X@&�@%@Ĝ@��@bN@�;@�w@��@;d@�y@�@�R@��@�+@ff@ff@V@@��@O�@��@�@(�@�
@�F@S�@@��@��@�\@~�@n�@n�@-@�@��@�7@G�@&�@&�@�@%@%@��@��@�9@1'@ �@ �@��@l�@l�@\)@K�@+@��@�y@�y@�y@�R@v�@5?@{@�@��@@�-@�-@�h@�@��@�@�D@Z@I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111114111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��hA��A� �A��;A�?}A��
A�?}A��AЮA�I�AΩ�AΏ\A�VA��Aˏ\A��/A�A�K�A���A�|�AāA��;A���A��/A�jA��A���A�jA���A��A�^5A��A���A��-A�K�A�1A��A�~�A�bNA�A��A�`BA��yA�%A�ƨA�ZA�ffA�G�A���A�bA�?}A��jA��hA�=qA��A��7A�O�A�(�A���A�S�A�7LA��!A�jA�`BA�O�A�jA�ƨA�ZA�9XA���A��7A��jA�-A���A��jA���A�+A�{A���A�A�ȴA�l�A��
A�~�A�+A�p�G�O�G�O�A��A��-A�1A��DA{�G�O�G�O�Ap�`Ah9XAd�AbĜAa�A]ƨAY+AXn�AXA�AU�PAQ�AP��AN �AJ��AH�AHz�AH  AG��AF��AE�AD�AC��AB=qAA33A@I�A?��A>�A<�RA;�#A:�`A9�A6I�A4�A41A3�A2-A1/A/�A-C�A+�A+�hA+l�A*1A( �A&�A%&�A#;dA!��AC�AJAVA�9A��G�O�G�O�A7LA�jA��A=qA�/A��A"�A �A"�A��A1A
�`A	��A	�#A	�;A	`BA  AȴAZA1A�-AA�RAVA�AG�A��Ar�A1'AA��Ap�A�PA�7Ax�AƨAx�A ȴA 1@�ƨ@��@��@��@�%@�V@�j@��w@���@�t�@��@��H@�E�@��#@��^@�7L@���@��9@�Z@��m@��@��#@�hs@�V@�(�@�!@���@��@�r�@��;@�@�u@�"�@���@�D@�9X@�!@�K�@��@�33@�$�@ش9@թ�@ԓu@�K�@�ff@�J@щ7@�t�@Η�@��`@˕�@ʧ�@�@�z�@Ǯ@�\)@�\)@�  @ʇ+@�b@���@��T@��@ͺ^@�x�@̋D@��@��@�1'@�%@�Ĝ@���@ũ�@��@�o@�J@��h@��7@�X@��@��@�x�@�(�@���@��@�V@���@���@���@�7L@�ƨ@��@��-@�%@���@��@�E�@���@�p�@��`@�1@�33@���@�ff@�n�@��@�&�@��@�r�@��@�I�@��F@�K�@��@���@�E�@�-@��!@���@�
=@�l�@��@�ȴ@�J@���@��^@��@��@��@�G�@� �@��P@�ȴ@�~�@�@��^@�hs@�/@���@���@��@�r�@�bN@�A�@� �@��@��@��P@�"�@�@��H@���@�n�@�J@���@��7@�7L@�r�@�9X@��m@�l�@�+@��@�M�@�5?@�E�@�E�@��@���@��7@�&�@���@��u@�1@��;@���@��w@���@�|�@�\)@�"�@��y@�~�@�^5@�J@��#@��^@���@�O�@�&�@��@��/@��D@�  @��w@���@�S�@�33@��@�ȴ@�^5@�$�@��@���@�`B@�7L@�&�@��@�V@��@��9@�(�@�  @���@�K�@�o@���@��+@�E�@�~�@���@���@���@�v�@�V@�5?@�{@��T@���@��h@�O�@�Ĝ@�z�@�Q�@�I�@�9X@�  @��@��@�dZ@�S�@�;d@��H@�ff@�V@�V@�=q@�-@��T@�%@���@���@��u@�Z@���@�dZ@�\)@�\)@�o@��!@�v�@�^5@�M�@�$�@��-@���@��h@��7@�X@�X@�?}@��/@���@�Q�@��;@�33@���@��H@�ȴ@�~�@�-@���@�%@��j@��j@��`@�%@�Ĝ@�A�@��F@�K�@�o@��y@��!@�v�@�M�@�E�@��@���@��7@�G�@��@��@���@��@��/@���@�j@�I�@� �@�1@�  @�1@�b@�b@��@K�@�@
=@~��@~5?@~{@~$�@}`B@|��@|�@}V@|�@|�D@|�@|j@{��@{C�@z�H@z�!@zM�@yhs@x�@x1'@w��@w��@w�P@w|�@wl�@w\)@wK�@v�+@u�@u/@t��@s��@s��@s�@s@r�!@r�\@r~�@r~�@rM�@q��@q��@q��@q&�@p �@ol�@o�@o;d@oK�@o;d@n�y@n5?@m��@m`B@m/@mV@l�@k33@j��@i�@h�@hA�@h  @g��@f�R@fE�@f{@e�@e@e�@d��@dZ@c�
@c�@ct�@cS�@c"�@b��@b^5@b-@a�#@a��@ax�@ahs@ax�@`�`@`��@` �@_�P@_�P@_\)@_+@^v�@]@]��@]�h@]�h@]�@]?}@]/@]�@]V@\��@\�/@\�/@\�j@\Z@\9X@[ƨ@[�F@[�F@[��@[t�@[dZ@Z��@Z^5@Z^5@Z^5@Zn�@Yx�@X��@X�u@Xr�@XQ�@X �@W;d@U�h@U�@T�/@T��@T�j@T�@T(�@T1@S��@SS�@R�@R��@R�!@Rn�@Q��@Q�7@Q&�@P��@P�@P1'@O�P@OK�@N��@Nff@N$�@N@M��@MO�@MV@Lz�@L1@K33@J�@J��@J�!@J^5@I��@I�^@I�7@I7L@HĜ@Hr�@H  @G+@F��@F�@F�R@F��@F5?@E�@E@E�-@E��@E�@D�/@Dj@C�@CdZ@CdZ@CC�@Co@B�H@B��@B^5@A��@Ax�@AX@@Ĝ@@�@@Q�@?�@?��@?�@?�@?�P@?�P@?�P@?l�@?;d@?
=@>�+@>V@=�@=@=@=p�@<�@<�@<z�@<(�@<�@;��@;�m@;t�@;33@;C�@;33@;o@;@;@;@:�@:�@:��@:n�@:-@9��@9��@9hs@9�@9%@8��@8�`@8��@8b@7��@7\)@7�@6�@6��@6�+@6v�@6ff@6E�@65?@6{@5�@5�-@5?}@5V@4�j@4�D@4z�@4Z@4(�@41@3��@3�F@3o@2��@2^5@2�@1��@17L@1�@1�@0��@0��@0Ĝ@0r�@0r�@0r�@0bN@0b@/�@/l�@/;d@/+@/�@/
=@.�R@.@-�T@-��@-V@,�j@,j@,(�@+�
@+�@+dZ@+S�@+"�@*n�@*J@)�7@)7L@)�@(��@(�9@(bN@(  @'�w@'�P@'\)@&�y@&ȴ@&�R@&��@&��@&��@&��@&ff@&E�@&{@%�T@%?}@$��@$�/@$�j@$��@$Z@$1@#��@#t�@#33@"�@"��@"n�@"^5@"M�@"J@!�@!�#@!��@!&�@!%@ �`@ bN@ Q�@ b@��@\)@;d@
=@�@�+@5?@{@�T@�@?}@/@�/@��@z�@j@I�@I�@��@��@t�@C�@�H@�H@�H@��@��@�!@��@~�@^5@^5@-@��@��@X@&�@%@Ĝ@��@bN@�;@�w@��@;d@�y@�@�R@��@�+@ff@ff@V@@��@O�@��@�@(�@�
@�F@S�@@��@��@�\@~�@n�@n�@-@�@��@�7@G�@&�@&�@�@%@%@��@��@�9@1'@ �@ �@��@l�@l�@\)@K�@+@��@�y@�y@�y@�R@v�@5?@{@�@��@@�-@�-@�h@�@��@�@�D@Z@I�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111114411111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	ƨB	ŢB	ŢB	ŢB	ƨB	ƨB	ǮB	ƨB	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ŢB	ŢB	�dB	�{B	�B	��B
 �B
33B
}�B
��B
�FB
�;B
=BoB �B8RBD�B�BD�B[#BYBN�BL�Be`Bs�B� B~�B|�B�7B�bB� B��B�9B�'B�!B�!B��B�#B�;B�HBBuB�B33BbNB�B�Bs�BdZBT�BXBC�B`BBjBe`BYBXB>wB'�BuBB��B��B�ZB��BɺB�B�)B�;B�)B��B�'B��B�7Bz�Bs�BgmBcTB8RB>wB49B;dB1'B&�B�B�BB  B
�yB
�B
R�B
\B	x�B	P�B	-B	{BɺB�PB�XB��B��B�3B��BƨBƨB��B�oB��B��B�=B��B��B�}B�qB�FB�B�jB�^B�B�RB�^B��B�9B��B�^B�FB�B��B�jBÖBÖBÖB�qB�B�XB�jB��B��B�}B�3B�XB��B�B�wB�!B�LB�?B��B��B��B�PB�TB�JB�Bx�Bv�Bz�B{�By�Bz�B�%B�VB�DB�B�{B��B�bB�+B{�B�7B�1B�+B�B�1B�1B�1B�B�+B�DB�DB�\B�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�3B�RB�LB�9B�?B�-B�3B�?B�3B�9B�FB�-B�!B�B�B�9B�9B�B��B��B��B��B�RB�B��B��B��B��B��B�=Bq�Bs�B�DB�1B� Bt�B�JB�oB��B��B��B�\B�uB�hB��B��B��B��B�'B��B��B�TB��B	+B	B��B��B��B��B�B�HB�B�mB��B��B�B�TB�B�yB��B��B	  B��B��B�B�fB�B��B��B��B��B	B	B��B��B�B	B	B��B	B	B	+B	%B	B	  B	  B	B	DB	VB	PB	DB	bB	�B	�B	�B	�B	�B	!�B	"�B	'�B	0!B	8RB	:^B	;dB	B�B	;dB	<jB	9XB	:^B	<jB	:^B	?}B	I�B	F�B	C�B	B�B	B�B	I�B	I�B	M�B	N�B	S�B	VB	YB	ZB	[#B	^5B	`BB	cTB	e`B	dZB	ffB	iyB	o�B	r�B	s�B	t�B	v�B	y�B	|�B	~�B	~�B	�1B	�=B	�DB	�\B	�oB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�-B	�9B	�FB	�LB	�FB	�RB	�^B	�RB	�XB	�XB	�jB	�}B	�wB	�}B	��B	�}B	��B	ÖB	ŢB	ƨB	ŢB	��B	��B	��B	��B	��B	ɺB	ǮB	��B	��B	��B	��B	��B	��B	��B	�)B	�)B	�#B	�)B	�#B	�/B	�5B	�5B	�5B	�BB	�;B	�;B	�5B	�HB	�`B	�mB	�fB	�`B	�ZB	�mB	�sB	�sB	�sB	�fB	�fB	�B	�B	�B	�B	�sB	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
B
B
B
B
%B
%B
%B
B
%B
1B
1B
DB
JB
PB
PB
PB
JB
JB
PB
VB
JB
JB
PB
VB
JB
DB
\B
hB
bB
oB
{B
oB
oB
hB
uB
uB
oB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
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
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
"�B
"�B
 �B
!�B
"�B
#�B
%�B
%�B
%�B
$�B
$�B
%�B
%�B
&�B
&�B
'�B
'�B
%�B
&�B
&�B
&�B
)�B
(�B
'�B
%�B
&�B
,B
,B
-B
,B
,B
-B
-B
-B
-B
,B
-B
,B
,B
.B
-B
/B
0!B
/B
/B
.B
-B
-B
0!B
0!B
.B
+B
+B
2-B
1'B
1'B
/B
,B
(�B
0!B
1'B
33B
49B
49B
33B
6FB
5?B
6FB
7LB
8RB
8RB
8RB
6FB
9XB
9XB
:^B
:^B
:^B
:^B
<jB
<jB
;dB
=qB
>wB
=qB
=qB
=qB
<jB
=qB
<jB
?}B
A�B
@�B
?}B
?}B
@�B
A�B
@�B
@�B
@�B
@�B
?}B
C�B
D�B
D�B
D�B
C�B
D�B
E�B
E�B
D�B
C�B
A�B
B�B
A�B
F�B
G�B
F�B
F�B
F�B
E�B
F�B
D�B
F�B
F�B
E�B
G�B
H�B
G�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
H�B
H�B
G�B
H�B
H�B
I�B
J�B
H�B
G�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
L�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
L�B
L�B
L�B
M�B
L�B
M�B
N�B
O�B
O�B
N�B
M�B
K�B
M�B
N�B
O�B
O�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
O�B
O�B
Q�B
Q�B
R�B
S�B
R�B
R�B
R�B
R�B
Q�B
O�B
Q�B
R�B
R�B
R�B
R�B
VB
W
B
VB
W
B
W
B
VB
XB
XB
W
B
VB
VB
W
B
XB
YB
XB
XB
VB
T�B
XB
W
B
VB
W
B
XB
YB
YB
YB
[#B
ZB
YB
W
B
YB
YB
[#B
]/B
]/B
\)B
[#B
\)B
]/B
^5B
^5B
]/B
_;B
`BB
`BB
aHB
`BB
`BB
_;B
_;B
_;B
_;B
^5B
`BB
aHB
aHB
aHB
aHB
`BB
aHB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
cTB
dZB
dZB
cTB
bNB
dZB
dZB
bNB
e`B
dZB
dZB
dZB
ffB
e`B
e`B
dZB
e`B
ffB
ffB
e`B
ffB
gmB
ffB
gmB
hsB
iyB
iyB
iyB
hsB
hsB
iyB
iyB
iyB
k�B
k�B
k�B
jB
jB
k�B
jB
jB
jB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
k�B
jB
l�B
l�B
k�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
l�B
l�B
m�B
l�B
n�B
m�B
n�B
o�B
o�B
o�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
q�B
s�B
s�B
r�B
r�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
t�B
u�B
v�B
v�B
v�B
w�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111114111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	ƨB	żB	ŢB	żB	ƨB	��B	ǮB	��B	ƨB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	ǮB	��B	��B	��B	�%B	�qB	��B	�vB	�B
$tB
8�B
��B
��B
��B
�\B
�BB#�B;�BH1BOBE�B[�BZ�BQhBPHBg�Bu?B�B�OBB�)B��B��B��B��B��B�GB��B�HB��B�vB��BSB�BB4Bb4B��B�gBvBgBXBZQBF�Ba-Bj�Bf2BZkBYBAUB*�B�B�B�cB��B�B��B�0B��B�~B�'B��B�pB��B�B��B}�Bv+Bi�Be,B<�B@�B5�B;�B2-B($B�BeB�BG�O�G�O�B
Y�B
�B	�gB	V�B	1'G�O�G�O�B��B�VB�$BѝB��B��B�_B��B��B��B��B�#B�"B��B��B�4B�BB��B� B�<B�B�B��B��B�'B��B�RB��B�B��B�B�B��B��B�B�.B��B�B�B�bBϑB��B��B��BÖB��B�iB�hB�$B��B�QB��B��G�O�G�O�B��B��Bz�Bx�B|PB}B{JB|B��B�\B��B�gB�{B��B��B��B}VB��B��B��B��B��B��B��B�B��B��B��B��B��B��B�yB��B��B��B�vB��B��B��B�B�'B�B�eB�WB�
B��B�lB�fB��B�tB��B��B�ZB��B��B�`B��B��B��B��B��B��B��B�B�B��B��B�	B�CB��B��B��B��B�'B��BtBu�B��B�B�oBv�B�B�@B�$B�!B�OB��B�FB��B�sB�dB�tB��B��B��B��B�B�9B	B	AB�B�B�$B�%B�]B�hB�6B�B�xB�$B�wB��B�B��B�ZB�6B	 B�B�LB�B��B�oB�DB�^B�jB�jB	B	aB�wB��B��B	aB	�B��B	{B	�B	zB	�B	�B	 �B	 �B	mB	�B	pB	�B	B	�B	�B	�B	�B	�B	�B	!�B	#:B	(>B	0!B	8B	:^B	;JB	B[B	;�B	<�B	9�B	:�B	<�B	:�B	?�B	IlB	F�B	DgB	CB	C-B	I�B	J#B	NB	O(B	T,B	V9B	Y1B	ZQB	[WB	^OB	`\B	cnB	ezB	d�B	f�B	i�B	o�B	r�B	s�B	uB	wB	zB	}<B	HB	�B	�fB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�4B	�B	�B	�"B	�6B	�)B	�)B	�WB	�IB	�IB	�GB	�aB	�nB	�FB	�fB	�`B	�lB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�"B	�B	�B	�,B	�2B	��B	�)B	�	B	�)B	�WB	�IB	�OB	�OB	�jB	�\B	ߊB	�pB	ޞB	�|B	�zB	�mB	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�+B	�?B	��B	�B	�B	�B	�B	��B	�'B	�B	��B	��B	��B	�6B	�>B	�>B	�0B	�B	�BB	�"B	�(B	��B
 B	�(B	�(B
 B
;B
'B
9B
?B
YB
?B
SB
YB
KB
1B
^B
JB
PB
PB
jB
~B
~B
jB
pB
~B
dB
jB
pB
�B
xB
\B
NB
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
B
�B
�B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
B
�B
�B
�B
�B
/B
 �B
"�B
#�B
"�B
"�B
!B
!�B
#B
#�B
%�B
&B
%�B
$�B
%B
%�B
%�B
'B
'B
'�B
'�B
&B
'B
'B
'B
)�B
)B
(
B
&2B
'8B
,B
,B
-B
,"B
,"B
-B
,�B
-B
-B
,"B
-B
,"B
,=B
./B
-)B
/B
0!B
/5B
/5B
./B
-CB
-CB
0!B
0!B
./B
+kB
+QB
2-B
1AB
1AB
/OB
,�B
)yB
0;B
1AB
33B
4TB
49B
3hB
6`B
5tB
6`B
7fB
8lB
8lB
8lB
6�B
9rB
9�B
:�B
:�B
:�B
:�B
<�B
<�B
;�B
=�B
>�B
=�B
=�B
=�B
<�B
=�B
<�B
?�B
A�B
@�B
?�B
?�B
@�B
A�B
@�B
@�B
@�B
@�B
?�B
C�B
D�B
D�B
D�B
C�B
D�B
E�B
E�B
D�B
C�B
A�B
B�B
A�B
F�B
G�B
F�B
F�B
F�B
E�B
F�B
D�B
F�B
F�B
E�B
G�B
H�B
G�B
I�B
I�B
J�B
J�B
J�B
J�B
I�B
H�B
H�B
G�B
H�B
H�B
I�B
J�B
H�B
G�B
J�B
J�B
J�B
K�B
K�B
K�B
J�B
L�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
L�B
L�B
L�B
M�B
L�B
M�B
N�B
O�B
O�B
N�B
M�B
K�B
NB
N�B
O�B
PB
Q B
Q�B
Q�B
RB
RB
Q�B
RB
RB
O�B
PB
RB
RB
SB
S�B
SB
SB
SB
S&B
RB
P.B
R:B
SB
SB
SB
S&B
VB
W
B
VB
W$B
W$B
VB
XB
XB
W$B
VB
VB
W$B
X+B
YB
XEB
X+B
V9B
U2B
X+B
W$B
V9B
W$B
X+B
YB
Y1B
Y1B
[#B
Z7B
Y1B
WYB
Y1B
Y1B
[=B
]/B
]IB
\CB
[=B
\CB
]IB
^jB
^5B
]dB
_;B
`BB
`BB
aHB
`BB
`BB
_VB
_VB
_VB
_VB
^OB
`\B
abB
abB
a|B
abB
`\B
abB
bhB
bhB
bhB
bhB
cnB
dZB
dZB
cnB
dtB
dtB
cnB
b�B
dtB
dtB
b�B
ezB
dtB
d�B
dtB
ffB
ezB
ezB
dtB
e`B
f�B
f�B
ezB
f�B
gmB
f�B
g�B
h�B
iyB
i�B
iyB
h�B
h�B
iyB
i�B
i�B
k�B
k�B
k�B
jB
jB
k�B
j�B
j�B
j�B
iyB
i�B
j�B
j�B
k�B
k�B
k�B
k�B
k�B
j�B
l�B
l�B
k�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
l�B
l�B
m�B
l�B
n�B
m�B
n�B
o�B
o�B
o�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
r�B
r�B
q�B
s�B
s�B
r�B
r�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
t�B
u�B
v�B
v�B
v�B
w�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111114411111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<9#�<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809180036142018091800361420180918003614201809180200262018091802002620180918020026201809190033592018091900335920180919003359  JA  ARFMdecpA19c                                                                20180914093513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180914003524  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180914003527  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180914003527  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180914003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180914003528  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180914003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180914003528  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180914003528  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180914003528  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180914003528  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180914003528                      G�O�G�O�G�O�                JA  ARUP                                                                        20180914005539                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180914153553  CV  JULD            G�O�G�O�F�	�                JM  ARSQJMQC2.0                                                                 20180915000000  CF  PSAL_ADJUSTED_QCCj  Cj  G�O�                JM  ARSQJMQC2.0                                                                 20180915000000  CF  TEMP_ADJUSTED_QCCj  Cj  G�O�                JM  ARCAJMQC2.0                                                                 20180917153614  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180917153614  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180917170026  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180918153359  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231518                      G�O�G�O�G�O�                