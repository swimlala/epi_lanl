CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-10-06T00:35:22Z creation;2016-10-06T00:35:24Z conversion to V3.1;2019-12-19T08:24:54Z update;     
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
resolution        =���   axis      Z        T  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  _�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  oH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  s    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     T  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     T  �L   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ڠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20161006003522  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               -A   JA  I2_0577_045                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��5�I2�1   @��6b� @3��'RTa�d�~���$1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @9��@�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�6f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A�ȴA�ȴA�ĜA�A���A߾wA߾wA���A���A���A�A�ĜA�ƨA�ȴA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���Aߧ�Aߡ�AߋDA�v�A�+AޓuA��yA�~�AԑhA�ZA҉7A�1A�JA��;A�ȴAϕ�A���AΉ7A�ffA���A˥�A�l�A��`A��;A���A��Aš�A�VA�"�Aò-A�A�-A�S�A���A��7A��A��HA�ZA���A��\A��/A���A�`BA�`BA��;A���A��A��`A��/A��TA�VA�p�A�M�A�ZA�(�A�jA��
A��/A�1A�dZA�v�A�Q�A�1A��jA��7A�n�A�
=A���A��A�dZA�1'A���A���A���A��wA�1A�=qA���A��!A��A�%A��
A��A�(�A��A���A��yA�=qA��!A��A�1A~ĜA|�!Az�AyAx1'At��Arr�Ao��AmhsAkt�AjE�AiAil�AiVAgS�Af��AfVAd�!AbbAadZA`r�A_XA^^5AZz�AX1'AV��AUARE�AO�mAN��AM�7AK�-AIO�AG��AF�jAFI�AE��ADA�ACABbA@ffA@bA?7LA> �A=�wA;C�A:�+A9�TA9��A8��A7�
A6�jA61A5x�A4�DA2JA/S�A-p�A+��A*��A)�A'�;A&ZA%dZA$bA#�A"��A!dZA �HA�^A��AQ�A`BA�jA�AƨA�A��A��A�+AQ�A-A�wA��A5?Av�A�A��AAXA��AI�A
�A
I�A	�A	%Ap�A�A��AffA �A�#A{Ar�A E�@��y@��h@�
=@�ff@�?}@�dZ@�-@�@�z�@��@�G�@��@�@��@��m@�\)@�v�@�-@�7L@�r�@�"�@���@�S�@��@�/@�bN@��@۶F@�C�@ڸR@�hs@�E�@�x�@��@�`B@�K�@�o@ְ!@�M�@�@�`B@�V@�Q�@�=q@ѩ�@���@ύP@θR@�~�@��@��@̴9@���@�V@�z�@˝�@�"�@ʸR@�p�@���@ǥ�@�n�@ŉ7@�&�@��/@�Ĝ@�1'@�\)@�;d@���@�n�@��@��^@�?}@��`@���@�  @���@�|�@�
=@��@��7@�&�@��9@�z�@�I�@�  @�S�@�^5@�J@���@�V@�@��!@�v�@�{@��7@�?}@��u@��@���@�{@�@�x�@�`B@�Q�@�\)@�C�@�o@��\@�ff@�5?@��T@��7@�&�@�Ĝ@��u@��@�A�@� �@�1@�Z@���@�(�@�|�@���@�=q@��@��#@��7@�G�@�?}@�7L@���@���@��9@�z�@�b@��
@���@�S�@�;d@�"�@��R@�~�@�v�@�E�@��@�O�@���@�I�@�b@��m@��F@���@��+@���@�
=@��@���@��+@�V@�5?@��T@�`B@�p�@���@�r�@�I�@�1'@��F@���@�  @��@�K�@�5?@�E�@�V@�@�X@�7L@��@���@��u@��@��@���@�+@�@�o@��T@��m@�^5@��^@�&�@�9X@�(�@��F@���@�n�@�5?@�V@���@�E�@��T@�`B@��7@�v�@�V@�^5@�E�@�@�`B@�`B@�O�@�/@�V@���@���@��@�1'@�b@�  @��F@�+@���@���@�~�@�5?@��T@���@�x�@�`B@�G�@�7L@�&�@���@���@�I�@���@��w@�|�@�\)@�\)@�K�@��y@��R@�=q@��@���@�?}@�p�@�hs@�X@��@���@���@�Z@��@��w@�K�@�@��@�ȴ@���@�$�@���@���@���@���@���@�hs@�O�@�/@��/@���@��u@�bN@��@�  @��m@���@��@�o@���@���@�V@�-@��@���@���@���@���@�`B@��`@�Ĝ@��u@�9X@��@�ƨ@��@�@��@��@��!@�M�@�J@��@���@���@�x�@�X@��@��/@���@��@�Q�@�(�@�  @��@l�@;d@�@~�R@~@}?}@|��@|�/@|�/@|j@{�
@z�\@zJ@y�^@y��@y�7@yhs@y&�@x��@x�u@w�@w|�@v��@v�@v�R@v�+@v$�@u��@u?}@uV@t�/@t�j@t�@t��@tj@tI�@t9X@t1@sS�@s@r��@rM�@r�@q�7@p��@pA�@p  @o�w@o�@n�@nff@m�-@m�@m�@l�@l(�@kt�@k"�@jM�@ihs@h�`@h�u@hQ�@h  @g��@gl�@g�@f��@fV@e@e�h@e�h@eO�@e�@d�@d��@d�D@d�@c"�@b~�@b�@a�^@a�^@a��@aG�@`Ĝ@`Q�@`1'@_��@_��@_�P@_l�@_�@^��@^{@]@]��@]`B@]/@\�/@\j@\1@[S�@Z��@Z�@ZJ@Y��@Y��@Y��@Y�^@Y��@Y�7@Y7L@X��@XA�@Wl�@Vȴ@V�+@Vv�@V5?@V{@U��@U?}@T�D@TI�@T�@S�m@S�
@S�
@S�@R��@Q��@Q�7@Q7L@PĜ@PQ�@O�@Ol�@O
=@Nȴ@N$�@M�-@Mp�@M?}@M�@MV@L��@L��@L9X@K�F@KdZ@KS�@K33@Ko@J��@J�\@JM�@J�@I�@I�^@Ix�@IX@I&�@H�`@HQ�@H1'@H  @H  @G�@G�@F�@FE�@F{@E@EO�@E/@D�/@D�@D�D@C�m@CS�@B��@B�!@B��@B~�@A��@A7L@@�`@@1'@?�@?��@?K�@?;d@?+@>�@>5?@=�@<��@<�@<��@<�D@<z�@<9X@<(�@;�
@;@:��@:M�@:-@9��@9hs@9&�@8��@8A�@8  @7�w@7l�@7
=@6�y@6ȴ@6ff@6@5�h@4��@4�D@4j@4Z@49X@4�@4�@41@3��@3�
@3�
@3�@333@2��@2^5@2J@1�^@1hs@1G�@17L@1%@0��@0��@0Ĝ@0�9@0Q�@/|�@.�y@.ȴ@.��@.E�@-�T@-��@-p�@-`B@-/@,z�@+�m@+�@+C�@+@*��@*�\@*~�@*~�@*n�@*M�@*M�@*-@*J@*�@)��@(�9@(b@(  @'�@'�@'��@'�w@'�w@'�@'|�@'K�@'+@&��@&ff@&{@%�T@%�h@%O�@$�/@$(�@#��@#ƨ@#S�@#"�@#@"��@"��@"��@"��@"�\@"n�@"-@"J@!�@!��@!G�@!&�@ ��@ �`@ ��@ Ĝ@ �@ Q�@ 1'@��@K�@�@
=@��@�@ȴ@ȴ@�R@ȴ@ȴ@�@�@ȴ@ȴ@ȴ@ȴ@ȴ@�R@��@v�@E�@$�@�T@O�@V@��@��@�D@I�@9X@(�@1@ƨ@��@dZ@�H@�\@^5@=q@��@�^@��@x�@&�@��@��@�u@�@Q�@A�@1'@b@��@\)@+@�@��@V@{@@�@`B@�@�@�D@Z@(�@1@�F@t�@S�@C�@"�@�@�H@��@��@��@��@�\@M�@�@J@��@�^@��@hs@7L@%@�9@�u@�@bN@A�@1'@�;@��@K�@��@�y@ȴ@�R@��@v�@$�@@�h@p�@`B@?}@/@�@/@�@�@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A�ȴA�ȴA�ĜA�A���A߾wA߾wA���A���A���A�A�ĜA�ƨA�ȴA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���Aߧ�Aߡ�AߋDA�v�A�+AޓuA��yA�~�AԑhA�ZA҉7A�1A�JA��;A�ȴAϕ�A���AΉ7A�ffA���A˥�A�l�A��`A��;A���A��Aš�A�VA�"�Aò-A�A�-A�S�A���A��7A��A��HA�ZA���A��\A��/A���A�`BA�`BA��;A���A��A��`A��/A��TA�VA�p�A�M�A�ZA�(�A�jA��
A��/A�1A�dZA�v�A�Q�A�1A��jA��7A�n�A�
=A���A��A�dZA�1'A���A���A���A��wA�1A�=qA���A��!A��A�%A��
A��A�(�A��A���A��yA�=qA��!A��A�1A~ĜA|�!Az�AyAx1'At��Arr�Ao��AmhsAkt�AjE�AiAil�AiVAgS�Af��AfVAd�!AbbAadZA`r�A_XA^^5AZz�AX1'AV��AUARE�AO�mAN��AM�7AK�-AIO�AG��AF�jAFI�AE��ADA�ACABbA@ffA@bA?7LA> �A=�wA;C�A:�+A9�TA9��A8��A7�
A6�jA61A5x�A4�DA2JA/S�A-p�A+��A*��A)�A'�;A&ZA%dZA$bA#�A"��A!dZA �HA�^A��AQ�A`BA�jA�AƨA�A��A��A�+AQ�A-A�wA��A5?Av�A�A��AAXA��AI�A
�A
I�A	�A	%Ap�A�A��AffA �A�#A{Ar�A E�@��y@��h@�
=@�ff@�?}@�dZ@�-@�@�z�@��@�G�@��@�@��@��m@�\)@�v�@�-@�7L@�r�@�"�@���@�S�@��@�/@�bN@��@۶F@�C�@ڸR@�hs@�E�@�x�@��@�`B@�K�@�o@ְ!@�M�@�@�`B@�V@�Q�@�=q@ѩ�@���@ύP@θR@�~�@��@��@̴9@���@�V@�z�@˝�@�"�@ʸR@�p�@���@ǥ�@�n�@ŉ7@�&�@��/@�Ĝ@�1'@�\)@�;d@���@�n�@��@��^@�?}@��`@���@�  @���@�|�@�
=@��@��7@�&�@��9@�z�@�I�@�  @�S�@�^5@�J@���@�V@�@��!@�v�@�{@��7@�?}@��u@��@���@�{@�@�x�@�`B@�Q�@�\)@�C�@�o@��\@�ff@�5?@��T@��7@�&�@�Ĝ@��u@��@�A�@� �@�1@�Z@���@�(�@�|�@���@�=q@��@��#@��7@�G�@�?}@�7L@���@���@��9@�z�@�b@��
@���@�S�@�;d@�"�@��R@�~�@�v�@�E�@��@�O�@���@�I�@�b@��m@��F@���@��+@���@�
=@��@���@��+@�V@�5?@��T@�`B@�p�@���@�r�@�I�@�1'@��F@���@�  @��@�K�@�5?@�E�@�V@�@�X@�7L@��@���@��u@��@��@���@�+@�@�o@��T@��m@�^5@��^@�&�@�9X@�(�@��F@���@�n�@�5?@�V@���@�E�@��T@�`B@��7@�v�@�V@�^5@�E�@�@�`B@�`B@�O�@�/@�V@���@���@��@�1'@�b@�  @��F@�+@���@���@�~�@�5?@��T@���@�x�@�`B@�G�@�7L@�&�@���@���@�I�@���@��w@�|�@�\)@�\)@�K�@��y@��R@�=q@��@���@�?}@�p�@�hs@�X@��@���@���@�Z@��@��w@�K�@�@��@�ȴ@���@�$�@���@���@���@���@���@�hs@�O�@�/@��/@���@��u@�bN@��@�  @��m@���@��@�o@���@���@�V@�-@��@���@���@���@���@�`B@��`@�Ĝ@��u@�9X@��@�ƨ@��@�@��@��@��!@�M�@�J@��@���@���@�x�@�X@��@��/@���@��@�Q�@�(�@�  @��@l�@;d@�@~�R@~@}?}@|��@|�/@|�/@|j@{�
@z�\@zJ@y�^@y��@y�7@yhs@y&�@x��@x�u@w�@w|�@v��@v�@v�R@v�+@v$�@u��@u?}@uV@t�/@t�j@t�@t��@tj@tI�@t9X@t1@sS�@s@r��@rM�@r�@q�7@p��@pA�@p  @o�w@o�@n�@nff@m�-@m�@m�@l�@l(�@kt�@k"�@jM�@ihs@h�`@h�u@hQ�@h  @g��@gl�@g�@f��@fV@e@e�h@e�h@eO�@e�@d�@d��@d�D@d�@c"�@b~�@b�@a�^@a�^@a��@aG�@`Ĝ@`Q�@`1'@_��@_��@_�P@_l�@_�@^��@^{@]@]��@]`B@]/@\�/@\j@\1@[S�@Z��@Z�@ZJ@Y��@Y��@Y��@Y�^@Y��@Y�7@Y7L@X��@XA�@Wl�@Vȴ@V�+@Vv�@V5?@V{@U��@U?}@T�D@TI�@T�@S�m@S�
@S�
@S�@R��@Q��@Q�7@Q7L@PĜ@PQ�@O�@Ol�@O
=@Nȴ@N$�@M�-@Mp�@M?}@M�@MV@L��@L��@L9X@K�F@KdZ@KS�@K33@Ko@J��@J�\@JM�@J�@I�@I�^@Ix�@IX@I&�@H�`@HQ�@H1'@H  @H  @G�@G�@F�@FE�@F{@E@EO�@E/@D�/@D�@D�D@C�m@CS�@B��@B�!@B��@B~�@A��@A7L@@�`@@1'@?�@?��@?K�@?;d@?+@>�@>5?@=�@<��@<�@<��@<�D@<z�@<9X@<(�@;�
@;@:��@:M�@:-@9��@9hs@9&�@8��@8A�@8  @7�w@7l�@7
=@6�y@6ȴ@6ff@6@5�h@4��@4�D@4j@4Z@49X@4�@4�@41@3��@3�
@3�
@3�@333@2��@2^5@2J@1�^@1hs@1G�@17L@1%@0��@0��@0Ĝ@0�9@0Q�@/|�@.�y@.ȴ@.��@.E�@-�T@-��@-p�@-`B@-/@,z�@+�m@+�@+C�@+@*��@*�\@*~�@*~�@*n�@*M�@*M�@*-@*J@*�@)��@(�9@(b@(  @'�@'�@'��@'�w@'�w@'�@'|�@'K�@'+@&��@&ff@&{@%�T@%�h@%O�@$�/@$(�@#��@#ƨ@#S�@#"�@#@"��@"��@"��@"��@"�\@"n�@"-@"J@!�@!��@!G�@!&�@ ��@ �`@ ��@ Ĝ@ �@ Q�@ 1'@��@K�@�@
=@��@�@ȴ@ȴ@�R@ȴ@ȴ@�@�@ȴ@ȴ@ȴ@ȴ@ȴ@�R@��@v�@E�@$�@�T@O�@V@��@��@�D@I�@9X@(�@1@ƨ@��@dZ@�H@�\@^5@=q@��@�^@��@x�@&�@��@��@�u@�@Q�@A�@1'@b@��@\)@+@�@��@V@{@@�@`B@�@�@�D@Z@(�@1@�F@t�@S�@C�@"�@�@�H@��@��@��@��@�\@M�@�@J@��@�^@��@hs@7L@%@�9@�u@�@bN@A�@1'@�;@��@K�@��@�y@ȴ@�R@��@v�@$�@@�h@p�@`B@?}@/@�@/@�@�@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bt�Bt�Bs�Bs�Bt�Bt�Bt�Bt�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bt�Bu�Bu�Bu�Bu�Bt�Bw�B~�B~�B~�B� B� B|�Bs�BhsBP�B�BiyBn�B^5BF�B5?BB�Bu�B�RB��B�B>wBjB�B��B�B�-B�LB�}BÖBŢB��B��BƨB��B�/B�B�B��B��B�B�B�B��B�BÖB�wB��Bw�BbNBS�BI�BA�B;dB2-B,BhBB �B5?B)�B�B��B��B�B� Br�BZBVBM�BI�BJ�BJ�BR�BJ�B1'B33B-B"�B
��B
�sB
�mB
�HB
��B
ȴB
ÖB
B
�wB
�3B
��B
��B
�uB
�bB
�=B
{�B
q�B
dZB
\)B
9XB
$�B

=B	��B	�B	�TB	�;B	�5B	�)B	�B	��B	��B	ȴB	�LB	�-B	��B	��B	��B	�JB	z�B	s�B	hsB	\)B	N�B	F�B	A�B	:^B	.B	'�B	 �B	�B	�B	�B	oB	VB	1B	B	B��B��B�B�B�B�sB�`B�NB�5B�#B�B��B��BÖB�wB�LB�9B�3B�!B��B��B��B��B��B��B��B��B�uB�uB�hB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�\B�{B��B��B�\B�7B�+B�PB�B|�B|�B{�B{�B~�B�1B�+Bx�Bu�Bt�Bt�Bu�Bv�Bw�Bx�B{�B{�B{�B}�B}�B� B�B�B�DB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�dB�wBÖB��B��B��B�
B�#B�)B�;B�BB�HB�`B�fB�yB�B�B�B��B��B��B��B��B��B��B��B	B	%B		7B	VB	\B	{B	�B	�B	�B	�B	�B	�B	�B	!�B	%�B	&�B	)�B	,B	.B	1'B	1'B	49B	6FB	;dB	=qB	@�B	E�B	I�B	M�B	Q�B	T�B	T�B	ZB	ZB	cTB	m�B	r�B	r�B	s�B	t�B	v�B	x�B	w�B	t�B	r�B	r�B	r�B	t�B	w�B	x�B	z�B	}�B	|�B	{�B	|�B	~�B	�B	�7B	�DB	�DB	�PB	�PB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�9B	�FB	�LB	�dB	�dB	�dB	�dB	�jB	�}B	��B	��B	��B	��B	��B	��B	B	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�B	�BB	�`B	�`B	�`B	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�fB	�HB	�BB	�5B	�)B	�)B	�;B	�5B	�HB	�ZB	�`B	�B	�B	�B	�B	�B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
1B
1B
1B
1B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
	7B
	7B
	7B
	7B
DB
JB
JB
PB
PB
PB
PB
JB
JB
DB
DB
DB
JB
JB
JB
JB
JB
PB
VB
bB
hB
oB
oB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
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
)�B
(�B
)�B
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
+B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
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
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
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
A�B
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
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
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
R�B
R�B
S�B
S�B
S�B
S�B
S�B
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
W
B
XB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
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
]/B
_;B
_;B
_;B
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
`BB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
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
l�B
l�B
l�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
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
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
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
s�B
s�B
s�B
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
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bt�Bt�Bs�Bs�Bt�Bt�Bt�Bt�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bt�Bu�Bu�Bu�Bu�Bt�Bw�B.BBHB�iB��B~�Bv�BoiB[=B"BkBo�B`'BH�B5�BC{BwLB��B�B�BABmB�+B��B��B��B�RB�OB��BȀB��B�BȚBЗB�;B�hB��B��B�	B��B�B��BּB�B�mB�B��Bz^Bd�BV9BK�BEmB>(B5%B0oB�BB"�B7B,qB!HBB�QB��B�-Bv�B[�BX_BO�BJ�BK�BLdBU�BM�B2|B5ZB0UB)DB�B
�B
��B
�B
��B
��B
�9B
�3B
��B
��B
�XB
��B
�FB
�:B
��B
~(B
shB
f�B
_�B
<�B
($B
�B	�B	��B	�B	��B	�!B	��B	��B	�B	�B	�^B	�lB	��B	��B	��B	��B	��B	}B	vzB	k�B	^�B	P�B	H�B	C�B	=B	/�B	)_B	!�B	�B	WB	+B	�B	B	�B	YB	aB�(B��B�B�cB�6B�B�B�B�;B�CB�B�9B�B�%B��B��B��B��B��B�eB�fB��B��B�]B��B�yB��B�MB��B�TB�:B�B�B��B��B�B��B��B�qB��B�	B�1B��B��B�
B�BB��B��B�jB�bB�=B�7B��B��B}qB}VB|jB|jB�B��B��Bz*Bv�BvFButBv�BxBy	Bz�B}�B}"B}"B.BB��B��B��B��B��B�pB�tB��B��B�)B��B�=B�OB�&B�2B�fB�yB��B��B��B�wB�MB��B�6B�@B�YB�qBܬBߤB�B�B��B�B�eB�B��B�B�?B��B��B��B�dB�}B�]B�}B	�B	�B	
#B	(B	�B	�B	�B	�B	B	)B	�B	B	B	"B	&2B	'RB	*KB	,WB	.cB	1[B	1�B	4�B	6�B	;�B	=�B	@�B	E�B	I�B	N"B	RoB	U�B	U2B	ZB	Y�B	c B	m�B	r�B	r�B	tB	uB	wfB	yrB	x�B	u%B	r�B	r�B	r�B	utB	xRB	y	B	{B	~BB	}"B	|B	}<B	HB	�mB	�lB	�^B	�^B	��B	�jB	�VB	�NB	�{B	��B	�B	�7B	�B	��B	��B	��B	�B	��B	�B	�8B	�
B	�B	�*B	�kB	�=B	�=B	�UB	�TB	�zB	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�uB	ňB	��B	��B	��B	�B	�B	�(B	�(B	��B	�.B	�4B	�B	��B	�&B	��B	�
B	�KB	��B	��B	�7B	�\B	��B	�B	�zB	�B	�B	�B	�B	�B	��B	��B	�B	��B	�}B	��B	�RB	��B	�B	޸B	�]B	ܒB	ߤB	ޞB	�bB	�@B	�FB	��B	��B	��B	�]B	�AB	��B	��B	�B
 4B
 OB
  B
'B
'B
-B
-B
GB
GB
MB
3B
3B
gB
�B
SB
SB
?B
YB
YB
_B
EB
KB
KB
fB
KB
fB
fB
�B
	lB

XB

rB

XB

XB

XB

�B

�B

�B
	lB
	lB
	�B
	RB
^B
dB
~B
�B
�B
jB
�B
�B
�B
xB
^B
xB
�B
�B
dB
dB
JB
PB
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
�B
�B
�B
 B
 'B
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
!�B
!�B
#B
# B
#B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
'B
'B
'B
'B
(
B
(
B
($B
(
B
)*B
)*B
)*B
*B
)*B
*0B
*B
*0B
+B
+B
+6B
+6B
,=B
,WB
,=B
,WB
,WB
+6B
+6B
+B
+B
+B
+6B
,"B
,WB
,"B
-CB
-)B
.B
./B
./B
/5B
/5B
/OB
0UB
0oB
0oB
1AB
1AB
1'B
1AB
1[B
2aB
2aB
3MB
3MB
33B
33B
3MB
4nB
4nB
4TB
5ZB
5ZB
5ZB
5ZB
5tB
6zB
6�B
6�B
7�B
7�B
8RB
8lB
8lB
8RB
8RB
8lB
8�B
8lB
8�B
8�B
9�B
9�B
9rB
9rB
9rB
9rB
9�B
:�B
:�B
;B
;�B
;dB
;dB
;B
;B
;�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?}B
?�B
?�B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
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
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
LB
MB
M�B
M�B
M�B
M�B
M�B
M�B
NB
N"B
N�B
N�B
N�B
OB
O�B
O�B
PB
P�B
Q B
Q B
QB
RB
RB
RB
R:B
R B
S&B
S&B
T,B
TB
TB
TB
TB
TB
S�B
S�B
TB
TB
UB
UB
U2B
U2B
UB
VB
W$B
W$B
W
B
W$B
W$B
W
B
W$B
W$B
W?B
WYB
X_B
Y1B
Y1B
Y1B
YB
Y1B
Y1B
Z7B
Z7B
ZkB
[WB
[=B
[=B
\CB
\CB
\CB
]/B
]/B
]IB
]IB
]/B
]IB
]IB
]B
]IB
]�B
_pB
_;B
_;B
_;B
_VB
_VB
_;B
_VB
_VB
`\B
`\B
`\B
`vB
`\B
abB
abB
abB
a�B
b�B
cnB
cnB
c�B
dtB
dtB
dtB
e`B
e`B
ezB
ezB
ezB
ezB
ezB
ezB
ezB
ezB
f�B
f�B
ffB
ffB
f�B
f�B
f�B
f�B
f�B
g�B
g�B
gmB
hXB
h�B
h�B
hsB
hsB
h�B
hXB
hsB
h�B
hsB
hsB
hsB
hXB
hsB
hsB
h�B
h�B
h�B
h�B
i�B
i�B
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
l�B
l�B
l�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
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
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
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
s�B
s�B
s�B
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
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%zx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201610100038572016101000385720161010003857201806221303092018062213030920180622130309201804050702432018040507024320180405070243  JA  ARFMdecpA19c                                                                20161006093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161006003522  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161006003522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161006003523  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161006003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161006003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161006003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161006003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161006003524  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161006003524                      G�O�G�O�G�O�                JA  ARUP                                                                        20161006012410                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161006153440  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20161009153857  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161009153857  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220243  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040309  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                