CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-11-29T00:35:20Z creation;2016-11-29T00:35:22Z conversion to V3.1;2019-12-19T08:20:41Z update;     
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pL   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݬ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �d   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �h   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �p   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20161129003520  20200116211515  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ?A   JA  I2_0577_063                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�ݵ�?�1   @�ݵ��@3%��n/�d�7Kƨ1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@���A   A   AA��A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@���A   A   AA��A`  A�  A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A՛�A՝�A՗�AՕ�Aՙ�A՝�A՟�Aա�Aա�A՟�A՟�Aե�Aե�Aղ-AնFA���A�ƨA���A���A���A���A��
A��`A�I�A�M�A���A�Q�A� �A�Q�A��;A���A�ȴAԅA�E�A�dZAҶFA�`BA��HA��A�bA�{A��A˟�A��/A���A�\)A�ĜA�(�A�bNA��A�ffA�  A��HA��uA��TA���A��A�r�A��TA��hA�?}A��A�1'A��A�A��#A�1A�O�A��yA�n�A���A���A��#A��A�ZA��`A�p�A���A�1'A���A�O�A��PA���A�7LA�bNA�ȴA�$�A���A�z�A���A�ZA���A��-A�|�A��\A�JA���A�A�A��;A�1'A�M�A���A�t�A�VA�E�A��#A�"�A�E�A�G�A��A�1'A{G�Aw��At-As�As�Aq|�Ao�TAn5?Al��AjffAg/Ad9XAa\)A_|�A^�HA]�#A[��AZ��AY��AWp�AT��AP��AN��AKS�AJbAI`BAH��AG�7AES�AChsAB�jAAXA@ffA?�PA>~�A=��A=��A<��A;�A:�A9l�A6�RA5XA4��A3�^A1;dA/��A-�mA,I�A+33A)��A(1'A&ĜA%&�A#��A"ĜA!p�A!oA bNA(�AbA�A�jA�A1'A�AA�HA��A�A�AĜA-A�7A��A �A�^A��A�A�A~�A��AG�A(�A/A
�A
v�A��AA�A{A�#A|�A��AG�A��A�9Az�A��AC�A�RA$�A/@��m@��+@�r�@��;@��@��T@�(�@�S�@�@�!@�1'@�@�^5@��/@�dZ@�\@��@噚@��/@��@�\@��@���@�~�@�X@�(�@ڏ\@���@�bN@ץ�@��@�/@ҏ\@�/@�"�@�@͙�@�G�@˕�@���@ʇ+@�V@�J@���@Ɂ@���@�Z@��m@�|�@�@�~�@�$�@�@ģ�@��@�E�@�p�@�I�@�
=@���@�n�@�V@�-@��@���@���@�G�@��@�z�@�1'@��F@�K�@�ȴ@�ff@��^@�hs@��@��`@�  @�
=@�ff@�$�@���@�`B@���@��@���@�j@��w@�+@��@�^5@��T@�X@���@�Z@�Q�@�(�@�"�@���@�hs@��@��@�1@�|�@�C�@�+@��R@��@�/@��
@�|�@�|�@��F@��w@�
=@�E�@�-@���@�$�@�{@�@���@�p�@��@��@��D@�bN@�Z@�A�@��F@�;d@�
=@��R@��+@�^5@�V@�M�@�=q@�=q@���@���@�x�@�O�@�&�@���@��j@��@�bN@�Q�@�9X@��@��m@��P@�l�@�S�@�;d@�+@���@��@�^5@��@�`B@��@���@��@��`@��/@���@�1@�l�@��H@��\@�v�@�n�@�^5@�V@��@���@�x�@�`B@�`B@�G�@�&�@���@���@��@��`@���@�O�@�&�@��9@��u@�z�@�Q�@�  @���@���@�ȴ@���@��H@�33@��F@��@�1'@�bN@��`@��@�I�@�1@��P@�\)@�C�@��@���@���@���@�n�@�=q@���@��h@��h@��@���@��#@��-@�&�@�j@���@���@��;@���@���@��P@�|�@�S�@���@�v�@�ff@�=q@�5?@�x�@�I�@�\)@��@��\@�V@�{@���@���@���@���@�V@�z�@��u@��@�bN@�bN@�1'@� �@�(�@� �@���@��@�33@�C�@�33@���@�n�@�$�@�@�M�@�E�@�hs@��7@���@�hs@�O�@�V@��/@�bN@�(�@��@��;@���@��F@�l�@�;d@��@�E�@��-@�x�@�G�@�7L@��/@�r�@�Q�@�b@���@�S�@���@��R@���@�v�@�@�@���@�x�@�`B@�V@��9@��D@�A�@� �@�b@�1@��@�P@�@~�+@~E�@~$�@~@}@}�h@}?}@|��@|�D@|j@|�@{t�@z�H@zn�@zJ@yG�@xA�@xb@w��@wK�@w;d@v��@v�+@u�-@uV@t�@tj@s��@s��@st�@st�@sdZ@so@r�\@r-@q�@q�#@q��@qx�@qX@qG�@q&�@p�u@o�P@n��@nff@m��@mV@l9X@l�@l1@l1@l1@l1@l1@k��@k�F@k��@kt�@kS�@k"�@j��@iG�@h �@g��@gl�@gK�@g�@f��@f@e��@e/@d��@d��@d�D@cdZ@b�!@b��@b��@bn�@a7L@`�9@`b@_�@_�P@_\)@_;d@^��@^�R@]�@]/@]V@[ƨ@Z�\@Z=q@Yhs@X��@X��@XA�@W�w@Wl�@V��@Vff@U�@U�@T�D@T1@S�F@S"�@R�H@R�\@R=q@Q��@Q%@O�;@O�P@OK�@O+@N�@N��@N@MO�@M?}@M�@L��@L�@K��@KdZ@K33@K@J��@I�7@H��@HĜ@H�9@H�u@H�@HbN@H �@G�@GK�@G�@Fv�@E�@E��@E�@E�@Ep�@Ep�@E`B@E?}@EV@D�/@DZ@Cƨ@CC�@B-@A��@AG�@A�@@��@@�9@@�u@@�@@b@?�;@?�@?|�@?|�@?|�@?l�@?\)@?K�@?+@>�+@>v�@>5?@=/@<�@<�/@<��@<�j@<�j@<�@<(�@;�m@;t�@:��@:-@9��@9X@9�@8�`@8�@7�;@7��@7�P@7|�@7;d@6E�@5�h@4�@4�j@4j@49X@3�@3C�@3"�@3"�@3"�@3"�@3o@2�H@2n�@2=q@2=q@2=q@2=q@2-@2J@1�#@1�^@1��@1hs@1%@0�9@0 �@/\)@.�R@.ff@.E�@.$�@-�@-�T@-�-@-�-@-��@-p�@-?}@,��@,�D@+��@+��@+33@*�H@*��@*~�@*J@)��@)��@)��@)��@)�^@)��@)��@)�7@)x�@)hs@)&�@(Ĝ@(�@(A�@(  @'�P@'+@'�@&�R@&E�@&@%�T@%@%�@%`B@%/@$�/@$�@$�D@$z�@$I�@$(�@#��@#�
@#��@#�@#�@#t�@#dZ@"�!@!�#@!��@!x�@!�@ �@ A�@ 1'@   @�@��@�@ff@$�@{@{@@��@/@z�@I�@(�@�m@��@C�@�@��@��@hs@G�@&�@��@�9@Q�@ �@b@�;@��@l�@+@��@ȴ@�R@��@�+@v�@ff@E�@$�@��@�-@p�@��@�j@�@��@��@��@��@�D@1@1@1@1@�
@��@��@t�@33@33@"�@@�@�H@�H@��@��@�!@^5@=q@-@J@�@�#@�#@�^@�^@�^@�7@x�@hs@hs@hs@X@G�@7L@�@%@��@�@r�@ �@�@�;@��@��@��@��@��@��@��@�w@|�@ȴ@v�@ff@V@V@V@E�@E�@E�@V@@`B@O�@?}@�@V@��@�/@��@��@�@I�@��@�
@��@�@t�@33@33@33@@
��@
^5@
M�@
M�@
=q@
-@	��@	��@	��@	x�@	&�@	&�@	�@��@�u@�@r�@bN@1'@�;@��@�w@�w@��@��@�w@��@|�@+@�y@�R@��@v�@ff@V@E�@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A՛�A՝�A՗�AՕ�Aՙ�A՝�A՟�Aա�Aա�A՟�A՟�Aե�Aե�Aղ-AնFA���A�ƨA���A���A���A���A��
A��`A�I�A�M�A���A�Q�A� �A�Q�A��;A���A�ȴAԅA�E�A�dZAҶFA�`BA��HA��A�bA�{A��A˟�A��/A���A�\)A�ĜA�(�A�bNA��A�ffA�  A��HA��uA��TA���A��A�r�A��TA��hA�?}A��A�1'A��A�A��#A�1A�O�A��yA�n�A���A���A��#A��A�ZA��`A�p�A���A�1'A���A�O�A��PA���A�7LA�bNA�ȴA�$�A���A�z�A���A�ZA���A��-A�|�A��\A�JA���A�A�A��;A�1'A�M�A���A�t�A�VA�E�A��#A�"�A�E�A�G�A��A�1'A{G�Aw��At-As�As�Aq|�Ao�TAn5?Al��AjffAg/Ad9XAa\)A_|�A^�HA]�#A[��AZ��AY��AWp�AT��AP��AN��AKS�AJbAI`BAH��AG�7AES�AChsAB�jAAXA@ffA?�PA>~�A=��A=��A<��A;�A:�A9l�A6�RA5XA4��A3�^A1;dA/��A-�mA,I�A+33A)��A(1'A&ĜA%&�A#��A"ĜA!p�A!oA bNA(�AbA�A�jA�A1'A�AA�HA��A�A�AĜA-A�7A��A �A�^A��A�A�A~�A��AG�A(�A/A
�A
v�A��AA�A{A�#A|�A��AG�A��A�9Az�A��AC�A�RA$�A/@��m@��+@�r�@��;@��@��T@�(�@�S�@�@�!@�1'@�@�^5@��/@�dZ@�\@��@噚@��/@��@�\@��@���@�~�@�X@�(�@ڏ\@���@�bN@ץ�@��@�/@ҏ\@�/@�"�@�@͙�@�G�@˕�@���@ʇ+@�V@�J@���@Ɂ@���@�Z@��m@�|�@�@�~�@�$�@�@ģ�@��@�E�@�p�@�I�@�
=@���@�n�@�V@�-@��@���@���@�G�@��@�z�@�1'@��F@�K�@�ȴ@�ff@��^@�hs@��@��`@�  @�
=@�ff@�$�@���@�`B@���@��@���@�j@��w@�+@��@�^5@��T@�X@���@�Z@�Q�@�(�@�"�@���@�hs@��@��@�1@�|�@�C�@�+@��R@��@�/@��
@�|�@�|�@��F@��w@�
=@�E�@�-@���@�$�@�{@�@���@�p�@��@��@��D@�bN@�Z@�A�@��F@�;d@�
=@��R@��+@�^5@�V@�M�@�=q@�=q@���@���@�x�@�O�@�&�@���@��j@��@�bN@�Q�@�9X@��@��m@��P@�l�@�S�@�;d@�+@���@��@�^5@��@�`B@��@���@��@��`@��/@���@�1@�l�@��H@��\@�v�@�n�@�^5@�V@��@���@�x�@�`B@�`B@�G�@�&�@���@���@��@��`@���@�O�@�&�@��9@��u@�z�@�Q�@�  @���@���@�ȴ@���@��H@�33@��F@��@�1'@�bN@��`@��@�I�@�1@��P@�\)@�C�@��@���@���@���@�n�@�=q@���@��h@��h@��@���@��#@��-@�&�@�j@���@���@��;@���@���@��P@�|�@�S�@���@�v�@�ff@�=q@�5?@�x�@�I�@�\)@��@��\@�V@�{@���@���@���@���@�V@�z�@��u@��@�bN@�bN@�1'@� �@�(�@� �@���@��@�33@�C�@�33@���@�n�@�$�@�@�M�@�E�@�hs@��7@���@�hs@�O�@�V@��/@�bN@�(�@��@��;@���@��F@�l�@�;d@��@�E�@��-@�x�@�G�@�7L@��/@�r�@�Q�@�b@���@�S�@���@��R@���@�v�@�@�@���@�x�@�`B@�V@��9@��D@�A�@� �@�b@�1@��@�P@�@~�+@~E�@~$�@~@}@}�h@}?}@|��@|�D@|j@|�@{t�@z�H@zn�@zJ@yG�@xA�@xb@w��@wK�@w;d@v��@v�+@u�-@uV@t�@tj@s��@s��@st�@st�@sdZ@so@r�\@r-@q�@q�#@q��@qx�@qX@qG�@q&�@p�u@o�P@n��@nff@m��@mV@l9X@l�@l1@l1@l1@l1@l1@k��@k�F@k��@kt�@kS�@k"�@j��@iG�@h �@g��@gl�@gK�@g�@f��@f@e��@e/@d��@d��@d�D@cdZ@b�!@b��@b��@bn�@a7L@`�9@`b@_�@_�P@_\)@_;d@^��@^�R@]�@]/@]V@[ƨ@Z�\@Z=q@Yhs@X��@X��@XA�@W�w@Wl�@V��@Vff@U�@U�@T�D@T1@S�F@S"�@R�H@R�\@R=q@Q��@Q%@O�;@O�P@OK�@O+@N�@N��@N@MO�@M?}@M�@L��@L�@K��@KdZ@K33@K@J��@I�7@H��@HĜ@H�9@H�u@H�@HbN@H �@G�@GK�@G�@Fv�@E�@E��@E�@E�@Ep�@Ep�@E`B@E?}@EV@D�/@DZ@Cƨ@CC�@B-@A��@AG�@A�@@��@@�9@@�u@@�@@b@?�;@?�@?|�@?|�@?|�@?l�@?\)@?K�@?+@>�+@>v�@>5?@=/@<�@<�/@<��@<�j@<�j@<�@<(�@;�m@;t�@:��@:-@9��@9X@9�@8�`@8�@7�;@7��@7�P@7|�@7;d@6E�@5�h@4�@4�j@4j@49X@3�@3C�@3"�@3"�@3"�@3"�@3o@2�H@2n�@2=q@2=q@2=q@2=q@2-@2J@1�#@1�^@1��@1hs@1%@0�9@0 �@/\)@.�R@.ff@.E�@.$�@-�@-�T@-�-@-�-@-��@-p�@-?}@,��@,�D@+��@+��@+33@*�H@*��@*~�@*J@)��@)��@)��@)��@)�^@)��@)��@)�7@)x�@)hs@)&�@(Ĝ@(�@(A�@(  @'�P@'+@'�@&�R@&E�@&@%�T@%@%�@%`B@%/@$�/@$�@$�D@$z�@$I�@$(�@#��@#�
@#��@#�@#�@#t�@#dZ@"�!@!�#@!��@!x�@!�@ �@ A�@ 1'@   @�@��@�@ff@$�@{@{@@��@/@z�@I�@(�@�m@��@C�@�@��@��@hs@G�@&�@��@�9@Q�@ �@b@�;@��@l�@+@��@ȴ@�R@��@�+@v�@ff@E�@$�@��@�-@p�@��@�j@�@��@��@��@��@�D@1@1@1@1@�
@��@��@t�@33@33@"�@@�@�H@�H@��@��@�!@^5@=q@-@J@�@�#@�#@�^@�^@�^@�7@x�@hs@hs@hs@X@G�@7L@�@%@��@�@r�@ �@�@�;@��@��@��@��@��@��@��@�w@|�@ȴ@v�@ff@V@V@V@E�@E�@E�@V@@`B@O�@?}@�@V@��@�/@��@��@�@I�@��@�
@��@�@t�@33@33@33@@
��@
^5@
M�@
M�@
=q@
-@	��@	��@	��@	x�@	&�@	&�@	�@��@�u@�@r�@bN@1'@�;@��@�w@�w@��@��@�w@��@|�@+@�y@�R@��@v�@ff@V@E�@E�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
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
��B  BBBB%B+B1B1BVBJ�B�JB��B�BbB"�B)�B/B6FBF�BT�Bl�Bs�Bs�Bs�Bu�B�1B�bB��B��B�3B�qBĜB��B�B�B�B�dB�^B�B�fB�5B�NB�BB�sB�B��B
=BB1BJBoB�BhBB��B�B�NB��B��B��B��B��B�oB�PB�+Bu�BffB\)BG�B%�B�B{BhBoBbB��B�`B�BǮB��B�uB�DB� By�BffB]/BF�B=qB:^B2-B�B%B
�B
�;B
��B
��B
��B
r�B
Q�B
8RB
0!B
-B
"�B
{B
1B	��B	�ZB	��B	�XB	��B	��B	�oB	�PB	� B	w�B	p�B	cTB	T�B	>wB	2-B	"�B	�B	�B	�B	bB		7B��B��B��B�B�B�mB�TB�NB�5B�B�B��B��BĜB��B�wB�LB�'B�B��B��B��B��B��B�{B�hB�bB�DB�=B�+B�B� B~�B|�B|�B|�Bz�Bz�By�Bx�Bw�Bv�Bs�Bp�Bp�Bn�Bl�Bl�Bm�Bs�Bo�Bn�Bk�BiyBgmBdZBcTBcTBe`BcTBbNBbNBaHBcTBaHB`BB`BB_;B`BB_;B^5B\)B_;B\)B\)B\)B[#B\)B[#BZBW
BT�BXBW
BS�BQ�BP�BT�BW
BXBYB[#B\)B_;Be`BhsBjBo�Bu�By�B|�B{�B{�B{�B�B�B�1B�PB�bB�uB��B��B��B�B�B�B�B�B�B�!B�'B�'B�'B�-B�-B�'B�9B�LB�RB�^B�qBBÖBŢBŢBƨBǮBȴBɺB��B��B��B��B��B��B�B�B�)B�/B�;B�BB�ZB�sB�B�B�B�B��B��B��B��B��B	  B	B	B	+B	DB	PB	bB	bB	bB	�B	�B	�B	�B	!�B	(�B	.B	0!B	1'B	5?B	9XB	>wB	D�B	F�B	G�B	J�B	L�B	O�B	R�B	VB	YB	^5B	_;B	aHB	bNB	dZB	ffB	iyB	hsB	hsB	hsB	hsB	l�B	n�B	o�B	p�B	q�B	r�B	t�B	u�B	u�B	u�B	w�B	z�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�+B	�7B	�=B	�DB	�DB	�JB	�PB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�LB	�RB	�RB	�LB	�LB	�LB	�FB	�LB	�RB	�XB	�^B	�dB	�wB	ÖB	ǮB	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�
B	�B	�B	�B	�B	�;B	�HB	�NB	�ZB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
+B
+B
+B
1B
1B
1B
	7B
DB

=B
	7B
1B
1B

=B

=B
	7B
DB
\B
bB
bB
\B
\B
\B
\B
bB
bB
bB
hB
oB
oB
oB
oB
hB
oB
oB
oB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
#�B
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
%�B
%�B
%�B
%�B
&�B
'�B
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
(�B
)�B
)�B
)�B
)�B
)�B
+B
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
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
49B
33B
33B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
=qB
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
A�B
A�B
A�B
A�B
A�B
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
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
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
J�B
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
K�B
K�B
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
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
P�B
Q�B
Q�B
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
VB
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
ZB
[#B
[#B
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
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
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
jB
jB
jB
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
q�B
q�B
q�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
u�B
u�B
u�B
u�B
u�B
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
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
w�B
x�B
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
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
|�B
|�B
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
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
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
��BBBB%B+BB�B<BJ�B�JB��B�-B�B#nB*B/iB6�BG�BV�Bm�Bt�Bu%BvBy�B�rB��B�CB��B�?B��B�B�eB��B�tB�tB�oB�B��B�$B�\B�B��B��B�B��BB�B�B�B@B7B�BMB��B�B�BՁB�=B�B��B��B�aB��B��By	Bh�B`\BLdB'�BB�B�B,B,B��B�RB�/B��B��B��B�dB��B|�Bh�B`�BG�B>�B<PB6+B"4B	�B
�B
�B
��B
żB
�B
v�B
UMB
9rB
1'B
/B
$�B
�B

�B	��B	�XB	�oB	��B	��B	��B	�B	��B	��B	y�B	s�B	g8B	Y1B	A�B	5�B	$ZB	�B	�B	�B	B	^B�.B�B��B��B��B�$B�B�B�pB��B�_B��B̈́B��B�[B�oB�rB��B�B��B��B��B��B��B�B�B��B�0B��B�	B�zB�;B}B}qB}�B}�B{�B{JBz�BzByrBy>Bt�Bq�Bq�BoiBmwBm�Bo Bt�BpUBo�Bl�Bj�Bh�Bd�BdZBeBf2Bc�Bb�Bc Bb�Bd�Ba�B`�B`�B`'BaHB`'B_;B]�B`�B]IB]dB\�B[�B]B\CB[	BX_BW?BY�BX�BVBSBQ�BU�BWsBX�BY�B[�B]/B`\BfLBiyBkkBp�Bv�Bz�B}qB|�B|�B}qB��B�SB��B�B��B�B��B�sB�*B�6B�QB�QB�kB��B��B��B��B��B��B��B��B�B�ZB��B�	B�0B�BB��B��B��B��B��B��B��B�	B�B�6B�(B�BB�NB�[B�yBٚB�xB�~BߊB��B��B��B��B� B��B�B��B��B�B�RB�PB	 OB	oB	�B	�B	�B	�B	}B	�B	4B	9B		B	B	 'B	"NB	)DB	.IB	0UB	1�B	5�B	:B	?HB	D�B	F�B	G�B	J�B	MPB	PHB	SB	VB	YB	^OB	_pB	abB	b�B	d�B	f�B	i�B	h�B	h�B	h�B	h�B	l�B	n�B	o�B	p�B	q�B	r�B	t�B	u�B	u�B	vB	xB	{B	|B	~B	.B	�4B	�AB	�-B	�3B	�3B	�SB	�YB	�_B	�RB	�XB	�^B	�^B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�B	��B	�B	�B	�B	�B	�$B	�0B	�B	�B	�"B	�"B	�"B	�B	�)B	�B	�!B	�B	��B	��B	�lB	�fB	��B	��B	��B	��B	�lB	�XB	�DB	�0B	�(B	�aB	ǔB	ʦB	͟B	� B	�MB	�9B	�YB	�$B	�B	�9B	�$B	�B	�+B	�KB	�QB	ߊB	�bB	�NB	�ZB	�LB	�B	��B	�B	�'B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�fB	��B	�fB	�B	�B	��B	��B	�B	��B	��B	��B	�DB	�6B	��B	�B	�B
B
'B
-B
+B
EB
_B
fB
�B
1B
	RB
xB

�B
	lB
KB
B

rB

�B
	7B
DB
�B
}B
�B
�B
�B
�B
�B
}B
}B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
 B
!B
!�B
!�B
!�B
!�B
!�B
!�B
"B
#B
#B
"�B
$B
#�B
#�B
#�B
#�B
$B
%B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&2B
&LB
'8B
($B
(>B
($B
)*B
)B
(�B
(�B
(�B
(�B
(�B
)B
)B
)B
*B
*B
*B
*KB
*B
+kB
,=B
-)B
-)B
-)B
.IB
.IB
/OB
/OB
/5B
/5B
/OB
/�B
0UB
0;B
0;B
0UB
0�B
1[B
2aB
2GB
2GB
2GB
2GB
2GB
2aB
2�B
4TB
3hB
3�B
5�B
6zB
6�B
7�B
7fB
7�B
7�B
8lB
8�B
8�B
9�B
9�B
9�B
:�B
:�B
;�B
;B
;B
;�B
;�B
<�B
=�B
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
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
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
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
J�B
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
K�B
LB
L�B
M�B
M�B
M�B
M�B
M�B
NB
M�B
NB
OB
OB
O�B
PB
O�B
O�B
PB
QB
Q B
Q�B
RB
QB
R:B
R:B
S&B
TB
TB
TB
T,B
UB
UB
T�B
T�B
T�B
UB
UB
U2B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
W?B
W?B
WYB
X_B
XEB
Y1B
Y1B
Y1B
Z7B
ZB
Z7B
ZB
Z7B
Z7B
Z7B
Z7B
ZQB
[WB
[WB
\CB
\CB
\CB
]IB
]IB
]/B
]/B
]IB
]/B
]/B
]/B
]/B
^5B
^5B
^OB
^OB
^OB
^OB
^OB
_VB
_pB
`\B
`\B
`vB
`vB
abB
abB
abB
abB
abB
bhB
bhB
bhB
bhB
bhB
bhB
cnB
cnB
cnB
cnB
cnB
cTB
cnB
cnB
c�B
d�B
ezB
ezB
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g�B
g�B
h�B
hsB
hsB
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
j�B
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
q�B
q�B
q�B
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
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
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
u�B
u�B
u�B
u�B
u�B
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
v�B
v�B
v�B
v�B
v�B
xB
w�B
w�B
w�B
x�B
w�B
x�B
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
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
|B
|B
|B
|B
}B
|�B
}B
}B
}B
}�B
}�B
~B
~B
~B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~B
~B
B
B
B
~�B
~�B
~�B
~�B
~�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912190820422019121908204220191219082042201804050705492018040507054920180405070549JA  ARFMdecpA19c                                                                20161129093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161129003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161129003521  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161129003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161129003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161129003522  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161129003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161129003522  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161129003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161129003522                      G�O�G�O�G�O�                JA  ARUP                                                                        20161129013146                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161129153304  CV  JULD            G�O�G�O�F���                JM  ARSQOW  1.1 2017V1                                                          20180404220549  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040541  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211515                      G�O�G�O�G�O�                