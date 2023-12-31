CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-17T00:35:19Z creation;2016-12-17T00:35:21Z conversion to V3.1;2019-12-19T08:19:33Z update;     
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
_FillValue                 �  I$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `t   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ܼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20161217003519  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               EA   JA  I2_0577_069                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��69D�1   @��6�`�@2�쿱[�d����+1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A��A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC	�fC�fC  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�<�D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�  A��A!��A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC	�fC�fC  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D6��D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�<�D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��fD��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bA�1A�JA�A���A���A�A�{A��A�&�A�/A�1'A�5?A�9XA�;dA�?}A�C�A�A�A�K�A�Q�A�VA�\)A�^5A�G�A�&�A�1A�M�AԲ-Aԉ7A�t�A�A���AҬA҃A���A�  A�+AѺ^A�bNA��A���A�A�=qA�=qA̼jA˅Aʝ�A�$�A�bA�-A�hsA�%A���A�E�A��A�VA��;A��HA��yA���A�K�A�5?A�  A��+A�A�A���A�ȴA��A�+A�1A��yA��A�&�A�?}A��A���A���A�XA��9A�|�A�O�A��A���A�Q�A�$�A�JA�r�A��A�I�A�\)A�"�A���A� �A��A��A��A��^A�A�A�`BA���A���A�+A�$�A�+A���A���A�"�A��yA�z�A�"�A���A�?}A}XA{K�AxJAvȴAv~�AvM�Au�At�AtJAs�FAp�9AjM�AiXAh��Ahn�Ag��Ae��AaƨAaoAaVAaVAaA`ffA[��AX�AW�mAWO�AV��AU��AT(�AQ�TAOG�AM�AJȴAI�AHI�AG��AG�AFE�AE;dAD��AC�A@�A=�7A;dZA9�A7��A61A4ȴA3hsA2jA1�wA0^5A-��A+��A*A(��A(ffA( �A'�;A'��A'G�A'oA&�RA$�/A$$�A#��A#l�A"�HA"A�A!;dA ��A ~�AA�DAv�AM�A�7AĜA��A1A(�Ap�AC�A�`AO�A�A�\A�RAbNAQ�AbA
�A	�-A��AjAVA�A�;A��A��A&�A��Ar�A�PA�/A^5A9XAbA�FA%A��AM�A�PA�A �9@�S�@��T@�?}@���@�@�p�@�&�@��@� �@��@��@�I�@��@�A�@�w@�E�@�@�@�@�Z@�o@��@���@�ƨ@�@�ff@�&�@؋D@�|�@��H@�J@ԣ�@��;@�C�@�{@Ѳ-@�Ĝ@ϝ�@���@�@̼j@̣�@���@�~�@��@�\)@Ə\@���@��@�/@�&�@��@Ĭ@���@�o@���@�&�@�I�@���@�t�@�$�@��j@��@��D@�ƨ@���@�x�@��/@��7@�@���@��7@��F@�E�@�-@�J@���@���@�hs@���@�9X@�Q�@�bN@��@�(�@�ƨ@�o@�@��@�G�@��/@��9@�b@��@���@�V@��@�@���@���@�bN@�1@��m@��w@���@�|�@��y@�v�@�J@��T@��^@�hs@�hs@��7@���@��`@���@��@���@���@��D@�b@��;@�S�@�C�@��T@�ff@���@�M�@��@�`B@�%@��/@��9@��@�t�@��P@�|�@�C�@�"�@��y@�n�@�-@�$�@��H@�33@�o@��@���@��+@�5?@���@�hs@��@�z�@��u@��@�Ĝ@��@��D@��@�I�@��m@��P@�33@�+@��@�M�@���@���@���@��7@���@��@��u@�Z@�9X@�b@�\)@�o@��@��\@���@�G�@�/@��@���@��9@�z�@�bN@�Q�@�9X@��@�1@���@��
@�dZ@�@��H@��!@�V@�V@�M�@�{@��T@��@���@���@�G�@�&�@��@���@���@��@�bN@�Z@�Q�@�  @�ƨ@���@���@���@�\)@�K�@�C�@��@��@���@��\@��#@��/@��u@��D@��D@��D@�9X@�ƨ@�;d@���@���@���@�ff@�=q@��@���@��7@��@��@�x�@�p�@�X@�?}@�/@�%@�I�@��@�  @��
@��P@��@�\)@�
=@�n�@���@�x�@�O�@�/@�%@��/@��9@��D@�bN@��m@�t�@��y@���@��\@�v�@�n�@�E�@�$�@���@��T@��#@���@�`B@�1'@�  @��;@��P@�\)@�"�@�ȴ@�V@��@�@���@�hs@��@���@���@��@�Z@�(�@�1@�@K�@�@~�y@~��@~5?@}�T@}@}�@}�@|I�@{��@z�@z~�@y��@yX@x��@x�@x �@w�@w;d@v�y@vff@u`B@t�/@t��@t��@tI�@sƨ@s�@sC�@s"�@so@s@r�H@q��@pr�@pQ�@pQ�@pA�@o�P@n��@nE�@m/@l�@l1@k�
@kS�@ko@j�!@j�\@j^5@i�@i��@ihs@i&�@h��@hr�@hQ�@h1'@g�@g�@g
=@f��@e�@e�h@d�j@cS�@b�!@b^5@b-@bJ@a�#@a��@a�^@a��@ax�@a&�@`��@_�@_�@_l�@^��@^�R@^�+@^5?@^{@]�@]��@]�@]/@\�D@\Z@\1@[��@[S�@["�@Z��@ZJ@Y�^@Y�7@YG�@Y&�@Y%@XĜ@X�u@Xr�@XQ�@X1'@W�@WK�@V�y@V��@U@U?}@U�@T��@Tz�@T9X@T�@T1@S��@S�
@St�@R��@R-@Q��@Q&�@P�u@Pr�@PQ�@PA�@P �@O�@Ol�@O
=@NV@N@M�h@M`B@MO�@M/@L��@L�@Kƨ@K�F@KdZ@J��@J�\@I��@I��@Ix�@Ihs@Ihs@H��@Hb@H  @G�@G�w@G�P@G\)@G
=@F�R@F��@F��@F@EO�@D��@Dj@D9X@C�F@Co@B��@B=q@B�@A�#@Ahs@A�@A�@@��@@��@@A�@@  @?�;@?|�@?\)@>��@>V@>{@=�@=@=�h@=/@<�/@<�D@<�D@<j@<�@;�m@;�@;33@;@:�H@:��@:=q@:J@9�^@9x�@9hs@8�`@8Q�@8 �@8b@7�w@7�P@7\)@7�@6�R@6ff@6{@5�T@5�@5/@5/@5V@4��@4�@4�/@4��@4��@4(�@3��@3C�@3"�@3@2�!@2M�@1��@1��@1��@0�`@0A�@0  @/�@/��@/�@/;d@.��@.�@.�@.�R@.�+@.ff@.5?@.{@-�T@-��@-�-@-��@-�@-`B@-/@,��@,�/@,�@,�D@,I�@+�
@+�F@+�@+o@*�H@*��@*^5@)�@)hs@)7L@(r�@'�;@'�@'��@'|�@'\)@'+@&�y@&��@&v�@&5?@%�T@%@%@%�-@%�-@%��@%`B@%�@$��@$�/@$��@$j@#��@#��@#��@#��@#33@"�\@"-@!��@!�^@!��@!��@!��@!hs@!%@ ��@ Q�@  �@�@��@|�@|�@��@K�@�R@V@�@�h@�h@�-@��@�h@`B@?}@�@�/@j@j@j@I�@(�@��@�@�@t�@S�@S�@@�\@�@�@J@��@��@��@x�@x�@x�@G�@�`@��@Ĝ@�@ �@�;@�P@\)@K�@�@�y@�@�@�R@�+@v�@v�@E�@$�@@�T@��@p�@O�@�@�/@�D@j@Z@9X@(�@1@�F@t�@"�@@�@�H@~�@�@��@��@x�@hs@X@G�@�@Ĝ@�9@�u@ �@  @�@�P@\)@;d@+@+@�@�y@�R@��@v�@5?@@�@��@�h@?}@V@V@�/@z�@(�@1@��@S�@o@
�@
�@
��@
�\@
~�@
^5@
=q@	�@	�@	�#@	�^@	�7@	X@	7L@	&�@	�@	%@��@��@�@r�@Q�@A�@Q�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�bA�1A�JA�A���A���A�A�{A��A�&�A�/A�1'A�5?A�9XA�;dA�?}A�C�A�A�A�K�A�Q�A�VA�\)A�^5A�G�A�&�A�1A�M�AԲ-Aԉ7A�t�A�A���AҬA҃A���A�  A�+AѺ^A�bNA��A���A�A�=qA�=qA̼jA˅Aʝ�A�$�A�bA�-A�hsA�%A���A�E�A��A�VA��;A��HA��yA���A�K�A�5?A�  A��+A�A�A���A�ȴA��A�+A�1A��yA��A�&�A�?}A��A���A���A�XA��9A�|�A�O�A��A���A�Q�A�$�A�JA�r�A��A�I�A�\)A�"�A���A� �A��A��A��A��^A�A�A�`BA���A���A�+A�$�A�+A���A���A�"�A��yA�z�A�"�A���A�?}A}XA{K�AxJAvȴAv~�AvM�Au�At�AtJAs�FAp�9AjM�AiXAh��Ahn�Ag��Ae��AaƨAaoAaVAaVAaA`ffA[��AX�AW�mAWO�AV��AU��AT(�AQ�TAOG�AM�AJȴAI�AHI�AG��AG�AFE�AE;dAD��AC�A@�A=�7A;dZA9�A7��A61A4ȴA3hsA2jA1�wA0^5A-��A+��A*A(��A(ffA( �A'�;A'��A'G�A'oA&�RA$�/A$$�A#��A#l�A"�HA"A�A!;dA ��A ~�AA�DAv�AM�A�7AĜA��A1A(�Ap�AC�A�`AO�A�A�\A�RAbNAQ�AbA
�A	�-A��AjAVA�A�;A��A��A&�A��Ar�A�PA�/A^5A9XAbA�FA%A��AM�A�PA�A �9@�S�@��T@�?}@���@�@�p�@�&�@��@� �@��@��@�I�@��@�A�@�w@�E�@�@�@�@�Z@�o@��@���@�ƨ@�@�ff@�&�@؋D@�|�@��H@�J@ԣ�@��;@�C�@�{@Ѳ-@�Ĝ@ϝ�@���@�@̼j@̣�@���@�~�@��@�\)@Ə\@���@��@�/@�&�@��@Ĭ@���@�o@���@�&�@�I�@���@�t�@�$�@��j@��@��D@�ƨ@���@�x�@��/@��7@�@���@��7@��F@�E�@�-@�J@���@���@�hs@���@�9X@�Q�@�bN@��@�(�@�ƨ@�o@�@��@�G�@��/@��9@�b@��@���@�V@��@�@���@���@�bN@�1@��m@��w@���@�|�@��y@�v�@�J@��T@��^@�hs@�hs@��7@���@��`@���@��@���@���@��D@�b@��;@�S�@�C�@��T@�ff@���@�M�@��@�`B@�%@��/@��9@��@�t�@��P@�|�@�C�@�"�@��y@�n�@�-@�$�@��H@�33@�o@��@���@��+@�5?@���@�hs@��@�z�@��u@��@�Ĝ@��@��D@��@�I�@��m@��P@�33@�+@��@�M�@���@���@���@��7@���@��@��u@�Z@�9X@�b@�\)@�o@��@��\@���@�G�@�/@��@���@��9@�z�@�bN@�Q�@�9X@��@�1@���@��
@�dZ@�@��H@��!@�V@�V@�M�@�{@��T@��@���@���@�G�@�&�@��@���@���@��@�bN@�Z@�Q�@�  @�ƨ@���@���@���@�\)@�K�@�C�@��@��@���@��\@��#@��/@��u@��D@��D@��D@�9X@�ƨ@�;d@���@���@���@�ff@�=q@��@���@��7@��@��@�x�@�p�@�X@�?}@�/@�%@�I�@��@�  @��
@��P@��@�\)@�
=@�n�@���@�x�@�O�@�/@�%@��/@��9@��D@�bN@��m@�t�@��y@���@��\@�v�@�n�@�E�@�$�@���@��T@��#@���@�`B@�1'@�  @��;@��P@�\)@�"�@�ȴ@�V@��@�@���@�hs@��@���@���@��@�Z@�(�@�1@�@K�@�@~�y@~��@~5?@}�T@}@}�@}�@|I�@{��@z�@z~�@y��@yX@x��@x�@x �@w�@w;d@v�y@vff@u`B@t�/@t��@t��@tI�@sƨ@s�@sC�@s"�@so@s@r�H@q��@pr�@pQ�@pQ�@pA�@o�P@n��@nE�@m/@l�@l1@k�
@kS�@ko@j�!@j�\@j^5@i�@i��@ihs@i&�@h��@hr�@hQ�@h1'@g�@g�@g
=@f��@e�@e�h@d�j@cS�@b�!@b^5@b-@bJ@a�#@a��@a�^@a��@ax�@a&�@`��@_�@_�@_l�@^��@^�R@^�+@^5?@^{@]�@]��@]�@]/@\�D@\Z@\1@[��@[S�@["�@Z��@ZJ@Y�^@Y�7@YG�@Y&�@Y%@XĜ@X�u@Xr�@XQ�@X1'@W�@WK�@V�y@V��@U@U?}@U�@T��@Tz�@T9X@T�@T1@S��@S�
@St�@R��@R-@Q��@Q&�@P�u@Pr�@PQ�@PA�@P �@O�@Ol�@O
=@NV@N@M�h@M`B@MO�@M/@L��@L�@Kƨ@K�F@KdZ@J��@J�\@I��@I��@Ix�@Ihs@Ihs@H��@Hb@H  @G�@G�w@G�P@G\)@G
=@F�R@F��@F��@F@EO�@D��@Dj@D9X@C�F@Co@B��@B=q@B�@A�#@Ahs@A�@A�@@��@@��@@A�@@  @?�;@?|�@?\)@>��@>V@>{@=�@=@=�h@=/@<�/@<�D@<�D@<j@<�@;�m@;�@;33@;@:�H@:��@:=q@:J@9�^@9x�@9hs@8�`@8Q�@8 �@8b@7�w@7�P@7\)@7�@6�R@6ff@6{@5�T@5�@5/@5/@5V@4��@4�@4�/@4��@4��@4(�@3��@3C�@3"�@3@2�!@2M�@1��@1��@1��@0�`@0A�@0  @/�@/��@/�@/;d@.��@.�@.�@.�R@.�+@.ff@.5?@.{@-�T@-��@-�-@-��@-�@-`B@-/@,��@,�/@,�@,�D@,I�@+�
@+�F@+�@+o@*�H@*��@*^5@)�@)hs@)7L@(r�@'�;@'�@'��@'|�@'\)@'+@&�y@&��@&v�@&5?@%�T@%@%@%�-@%�-@%��@%`B@%�@$��@$�/@$��@$j@#��@#��@#��@#��@#33@"�\@"-@!��@!�^@!��@!��@!��@!hs@!%@ ��@ Q�@  �@�@��@|�@|�@��@K�@�R@V@�@�h@�h@�-@��@�h@`B@?}@�@�/@j@j@j@I�@(�@��@�@�@t�@S�@S�@@�\@�@�@J@��@��@��@x�@x�@x�@G�@�`@��@Ĝ@�@ �@�;@�P@\)@K�@�@�y@�@�@�R@�+@v�@v�@E�@$�@@�T@��@p�@O�@�@�/@�D@j@Z@9X@(�@1@�F@t�@"�@@�@�H@~�@�@��@��@x�@hs@X@G�@�@Ĝ@�9@�u@ �@  @�@�P@\)@;d@+@+@�@�y@�R@��@v�@5?@@�@��@�h@?}@V@V@�/@z�@(�@1@��@S�@o@
�@
�@
��@
�\@
~�@
^5@
=q@	�@	�@	�#@	�^@	�7@	X@	7L@	&�@	�@	%@��@��@�@r�@Q�@A�@Q�@Q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
�B
��B
�B
�'B
�3B
�LB
�RB
�!B
��B�B��B�RB�dB�#BJB\)BffBr�Be`Bl�B�Bz�Bq�Bu�Bt�Bs�Bs�B�B�=B�\B�VB�DB�VB��B��B�B��B�RBǮB�BB�B�sB�fB�NB�B�B��BɺB��B�LB��B��B��B��B��B�B��B��B��B�uB�DB�1B�B�B~�B|�Bx�BffBP�B;dB49B"�B�B  B�B�B�B��BÖBÖB�wB�LB�B�{B�Bk�BH�BC�B=qB)�B{B
��B
�fB
ĜB
�B
�bB
s�B
[#B
C�B
:^B
8RB
7LB
6FB
1'B
+B
%�B
�B	�B	�fB	�;B	�B	��B	�}B	�9B	�?B	�FB	�RB	�RB	�9B	��B	�DB	�B	~�B	z�B	s�B	hsB	^5B	L�B	A�B	49B	,B	%�B	"�B	�B	�B	�B	uB	hB	B��B�B�mB�fB�HB�NB�NB�;B�/B�B��BÖB�wB�dB�^B�XB�^B�dB�dB�dB�^B�jB��BĜBŢBŢBɺB��BɺBȴB��B�dB�XB�XB�LB�9B�B��B��B��B��B�uB��B�\B�PB�DB�=B�VB�VB�hB�bB�DB�=B�7B�7B�7B�7B�1B�=B�7B�7B�DB�PB�PB�PB�PB�PB�VB�PB�\B�hB�\B�\B�\B�\B�\B�uB��B��B��B��B��B��B�oB��B�oB�\B�bB��B��B��B��B��B��B��B��B�B�B�B�'B�LB�LB�LB�RB�dB�dB�qB��BÖBƨBɺB��B��B��B�B�)B�HB�;B�/B�TB�B�B��B��B��B	B	%B	1B	B	B	+B	PB	�B	�B	�B	�B	�B	�B	�B	�B	�B	(�B	.B	0!B	33B	8RB	6FB	6FB	7LB	8RB	:^B	=qB	D�B	I�B	L�B	M�B	N�B	VB	YB	[#B	YB	XB	YB	YB	\)B	aHB	gmB	hsB	jB	k�B	k�B	m�B	q�B	s�B	t�B	u�B	u�B	u�B	v�B	y�B	{�B	~�B	� B	�B	�%B	�1B	�DB	�\B	�\B	�\B	�\B	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�-B	�3B	�FB	�dB	��B	��B	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�)B	�5B	�;B	�;B	�;B	�;B	�5B	�5B	�5B	�;B	�HB	�HB	�BB	�BB	�BB	�BB	�HB	�HB	�HB	�HB	�BB	�NB	�NB	�NB	�NB	�ZB	�fB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B	��B	��B	��B
  B	��B	��B
  B
B
B
B
B
B
B
%B
%B
%B
+B
1B
1B
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
%B
B
B
B
B
%B
+B
+B
1B
1B

=B
DB
JB
PB
PB
PB
VB
VB
\B
bB
bB
bB
bB
hB
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
'�B
'�B
'�B
'�B
'�B
'�B
(�B
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
-B
-B
-B
-B
-B
-B
.B
.B
/B
.B
.B
/B
/B
/B
0!B
0!B
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
2-B
33B
33B
33B
33B
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
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
>wB
?}B
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
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
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
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
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
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
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
S�B
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
XB
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
[#B
\)B
\)B
\)B
\)B
\)B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
bNB
aHB
bNB
bNB
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
dZB
e`B
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
iyB
iyB
iyB
hsB
iyB
iyB
iyB
jB
jB
iyB
iyB
iyB
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
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
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
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
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
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
�B
��B
�B
�'B
�3B
�LB
�RB
��B
��B�B��B��B��B�B�B\xBf�Bs3BezBl�B��B{�Br|Bv�Bv�Bu�BvFB�B��B�NB�}B��B�uB��B�fB��B�IB��B�7B� B�B�B�sB�:BچB��B�&B�DB��B�JB�@B�]B�HB��B��B��B��B��B��B�MB�0B�lB��B��B�B~wB{�BiBS@B=VB6�B%BBaB� B�!B��B�$BżB�B�B��B��B�sB��Bo�BKBESB@4B-wB�B
��B
�kB
ȀB
�iB
��B
vzB
^OB
EB
:�B
8�B
8B
7�B
2-B
,�B
*KB
#TB	�3B	�8B	�B	ۦB	ѝB	�-B	��B	�ZB	�zB	�	B	�^B	�XB	��B	��B	�B	� B	|�B	u�B	k�B	abB	O�B	D3B	6+B	-)B	&�B	#�B	 �B	�B	�B	�B	MB	B��B�!B��B�XB� B�B�B�B�pB�]B�\BňB��B�B��B��B��B��B��B�jB�jB�VB� B�9B�tBƨB��B�xB�)BˬB��B��B��B��B��B�2B�5B��B�B�B��B��B�YB��B�BB��B��B�(B��B��B��B��B��B��B��B��B��B��B��B��B�rB�0B��B��B��B��B�<B��B�"B�bB� B�HB��B�HB��B�bB�,B�B��B�B��B�qB�KB�aB��B��B�B��B��B�yB��B��B��B��B��B��B��B��B��B��B�B��B�B�>B�B�B�BB�B�MB�zB�rB̘B�pB�NB��B�/B�NB�'BݲB�nB�B�'B��B��B�wB	�B	�B		B	�B	�B	�B	jB	sB	QB	�B	�B	1B	kB	CB	B	�B	)B	.IB	0�B	4nB	9$B	6`B	6zB	7�B	8�B	:�B	=�B	EB	I�B	L�B	M�B	OBB	VmB	Y�B	[�B	Y�B	X_B	YeB	YKB	\�B	a�B	g�B	h�B	j�B	k�B	k�B	nB	rB	s�B	t�B	u�B	u�B	u�B	w2B	z*B	|6B	B	�B	�AB	�%B	�KB	��B	�vB	�vB	�vB	�vB	�vB	��B	��B	��B	��B	��B	�IB	�qB	��B	�FB	�>B	�_B	�6B	�6B	�6B	�kB	�QB	�B	�/B	�[B	�aB	�hB	��B	�B	�iB	�;B	ȚB	��B	��B	� B	� B	�@B	�MB	�mB	�mB	�$B	��B	��B	�CB	�OB	�;B	�VB	�pB	ߊB	ބB	�jB	�5B	ߊB	�B	�|B	�\B	�\B	��B	��B	�bB	�|B	�|B	�bB	�vB	�B	�B	�B	�B	��B	�B	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�(B	�(B	�.B	�.B	�HB	��B
 �B
 4B	�B	�B	�B	�BB	�cB
 OB	�cB	�.B
 B
 B
[B
GB
GB
B
B
%B
%B
?B
EB
KB
KB
_B
�B
_B
EB
zB
_B
EB
_B
�B
�B
�B
3B
3B
SB
SB
?B
EB
_B
fB
�B

�B
�B
~B
jB
jB
jB
pB
pB
vB
�B
�B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
B
 B
 B
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
#B
#�B
#�B
#�B
$B
#�B
$�B
%B
$�B
$�B
%B
%,B
&LB
'�B
'�B
(
B
(>B
(>B
(>B
)_B
*0B
*B
+B
+6B
+B
+B
,"B
,"B
,=B
,B
,"B
,"B
-CB
-)B
-)B
-)B
-)B
-CB
-CB
.IB
.IB
/OB
.}B
.}B
/OB
/5B
/5B
0;B
0;B
0!B
0!B
0;B
0UB
0;B
0UB
1[B
1AB
1AB
2aB
2GB
2GB
2GB
3MB
3MB
3MB
3MB
3hB
3hB
4TB
4TB
4nB
4TB
4TB
4nB
5tB
5ZB
5ZB
6zB
6`B
6zB
6`B
6FB
6FB
6`B
6`B
7�B
7�B
7�B
7�B
7�B
8�B
8lB
8lB
9rB
9rB
:xB
:DB
:xB
:xB
:xB
:�B
;�B
;�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
>�B
?�B
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
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
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
F�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
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
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
O(B
OB
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
Q B
Q B
Q B
Q B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
RB
R B
R B
SB
R�B
SB
SB
TB
S�B
TB
TB
TFB
U2B
UB
VB
VB
VB
VB
W$B
W$B
W
B
W$B
X+B
X+B
X+B
X+B
Y1B
YB
Y1B
YB
YB
Y1B
Y1B
Y1B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
Z7B
[=B
[=B
[=B
\CB
\]B
\]B
\CB
\xB
^jB
^OB
^5B
^OB
^OB
_VB
_VB
`\B
`\B
`\B
`\B
`\B
`\B
`BB
`BB
`BB
`\B
aHB
abB
bhB
abB
bhB
b�B
cnB
cTB
cnB
c�B
d�B
dtB
d�B
dZB
dZB
dZB
d@B
dtB
ezB
f�B
f�B
ffB
f�B
g�B
g�B
gmB
gmB
h�B
h�B
i�B
i�B
i�B
hsB
iyB
iyB
i�B
jB
jB
i�B
i�B
i�B
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
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
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
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
zB
y�B
y�B
y�B
z�B
z�B
z�B
z�B
|B
{�B
{�B
|B
|B
{�B
{�B
|B
|B
}B
}B
}B
}�B
~B
~B
~B
~B
~B
~B
}�B
}�B
}�B
}�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612210036302016122100363020161221003630201806221306302018062213063020180622130630201804050706482018040507064820180405070648  JA  ARFMdecpA19c                                                                20161217093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161217003519  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161217003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161217003520  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161217003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161217003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161217003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161217003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161217003521  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161217003521                      G�O�G�O�G�O�                JA  ARUP                                                                        20161217013314                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161217153645  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20161217153645  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20161220153630  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161220153630  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220648  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040630  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                