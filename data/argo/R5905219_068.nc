CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-02-29T06:37:53Z creation;2020-02-29T06:37:56Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200229063753  20200229065423  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               DA   JA                                  2B  A   APEX                            7906                            051216                          846 @����Y�1   @������@4���S��d���vȴ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@���A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@ffBHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C� C�3C  C  C
  C  C  C  C  C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C��D fD � D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2�fD3  D3y�D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<fD<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG�fDH  DH� DI  DIy�DI��DJ� DK  DK� DL  DLy�DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D D¼�D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ Dȼ�D�  D�@ Dɀ D�� D�  D�<�Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�3D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�<�D�|�D�� D�  D�@ D�|�DԼ�D�  D�@ D�|�D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dރ3D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D��3D�� D�  D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�fgA��A$��AD��Ad��A�ffA���A�ffA�ffA�ffA�ffA�ffA�ffB33B	33B33B33B ��B)33B133B933BA��BI��BQ33BY33Ba33Bi33Bq33By33B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�Bș�B̙�BЙ�Bԙ�B���Bܙ�B���B䙚B虚B왚B�B���B���B���C L�C��C  CL�CL�C
L�CL�CL�CL�CL�C33CL�CL�CL�CL�CL�C L�C"L�C$L�C&L�C(L�C*L�C,L�C.L�C0L�C2L�C4L�C6L�C8L�C:L�C<L�C>L�C@L�CBL�CD33CF33CHL�CJL�CLL�CNL�CPL�CRL�CTL�CVL�CXL�CZL�C\L�C^L�C`L�CbL�CdL�CfL�ChL�CjL�ClL�CnL�CpL�CrL�CtL�CvL�CxL�CzL�C|L�C~L�C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�&fC�&fC�&fC�&fC��C��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC��C�&fC�&fC��C�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�&fC�33C�33C�&fC�&fC�&fC�&fC�&fC�33C�33D �D �3D3D�3D3D�3D3D�3D3D��D3D�3D3D�3D3D�3D3D�3D	3D	�3D
3D
�3D3D�3D3D�3D3D�3D�D��D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D3D�3D�D�3D3D�3D 3D �3D!3D!�3D"3D"�3D#3D#�3D$3D$��D%�D%�3D&3D&�3D'3D'�3D(3D(�3D)3D)�3D*3D*�3D+3D+�3D,3D,�3D-3D-�3D.�D.�3D/3D/�3D03D0�3D13D1�3D23D2��D33D3��D43D4�3D53D5�3D63D6�3D73D7�3D8�D8�3D93D9�3D:3D:�3D;3D;�3D<�D<�3D=3D=�3D>3D>�3D?3D?�3D@3D@�3DA3DA�3DB�DB�3DC3DC�3DD3DD�3DE3DE�3DF3DF�3DG�DG��DH3DH�3DI3DI��DJ�DJ�3DK3DK�3DL3DL��DM3DM�3DN�DN�3DO3DO�3DP3DP�3DQ3DQ�3DR3DR�3DS3DS�3DT3DT�3DU3DU�3DV3DV�3DW3DW�3DX3DX�3DY3DY�3DZ3DZ�3D[3D[�3D\3D\�3D]3D]�3D^3D^�3D_3D_�3D`3D`�3Da3Da�3Db3Db�3Dc3Dc�3Dd3Dd�3De3De�3Df3Df�3Dg3Dg�3Dh3Dh��Di3Di�3Dj3Dj�3Dk�Dk�3Dl3Dl�3Dm3Dm��Dn3Dn�3Do3Do�3Dp3Dp�3Dq3Dq�3Dr3Dr�3Ds3Ds�3Dt3Dt�3Du�Du�3Dv3Dv�3Dw3Dw�3Dx3Dx�3Dy3Dy�3Dz3Dz�3D{3D{�3D|3D|�3D}3D}�3D~3D~�3D3D�3D�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D���D�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�gD�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD��D�L�D���D�ɚD�	�D�I�D���D���D�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D��gD�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D��gD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D��gD�	�D�L�D���D�ɚD�	�D�I�D�D��gD�	�D�I�DÉ�D�ɚD�	�D�I�Dĉ�D�ɚD�	�D�I�Dŉ�D�ɚD�	�D�I�DƉ�D�ɚD�	�D�I�Dǉ�D�ɚD�	�D�I�Dȉ�D��gD�	�D�I�Dɉ�D�ɚD�	�D�FgDʉ�D�ɚD�	�D�I�Dˉ�D�ɚD�	�D�I�D̉�D�ɚD�	�D�I�D͉�D�ɚD�	�D�I�DΉ�D�ɚD��D�I�Dω�D�ɚD�	�D�I�DЉ�D�ɚD�	�D�I�Dщ�D�ɚD�	�D�I�D҉�D�ɚD�	�D�FgDӆgD�ɚD�	�D�I�DԆgD��gD�	�D�I�DՆgD�ɚD�	�D�I�D։�D�ɚD�	�D�I�D׉�D�ɚD�	�D�I�D؉�D�ɚD�	�D�I�Dى�D�ɚD�	�D�I�Dډ�D�ɚD�	�D�I�Dۉ�D�ɚD�	�D�I�D܉�D�ɚD�	�D�I�D݉�D�ɚD�	�D�I�Dތ�D�ɚD�	�D�I�D߉�D�ɚD�	�D�I�D���D�ɚD�	�D�I�DቚD�ɚD�	�D�I�D≚D�ɚD��D�I�D㉚D�ɚD�	�D�I�D䉚D�ɚD�	�D�I�D剚D�ɚD�	�D�I�D扚D�ɚD�	�D�I�D牚D�ɚD�	�D�I�D艚D�ɚD�	�D�I�D鉚D�ɚD�	�D�I�DꉚD�ɚD�	�D�I�D뉚D�ɚD�	�D�I�D쉚D�ɚD�	�D�I�D퉚D�ɚD�	�D�I�DD�ɚD�	�D�I�DD��gD�	�D�I�D���D�ɚD�	�D�I�D�D�ɚD�	�D�I�D�D�ɚD�	�D�I�D�D�ɚD�	�D�I�D�D�ɚD�	�D�I�D���D�ɚD�	�D�I�D���D�ɚD�	�D�I�D��gD�ɚD�	�D�I�D���D�ɚD�	�D�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;dA�;dA�9XA�33A�7LA�;dA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�7LA�7LA�/A�-A�+A�+A�+A�&�A�&�A�&�A�(�A�(�A�(�A�&�A�(�A�+A�+A�+A�-A�-A�-A�-A�-A�/A�/A�1'A�1'A�1'A�/A�1'A�33A�33A�5?A�7LA�7LA�7LA�7LA�7LA�33A�33A�33A�33A�33A�33A�33A�5?A�/A��TAɶFA�XAƲ-A��
A��;A�VA���A�-A��A���A���A��A���A��A���A�n�A�(�A�ffA�p�A�ƨA���A�O�A��wA�ȴA�\)A�G�A�z�A��wA���A�XA���A��/A���A�n�A��A�-A�1'A�K�A�G�A�%A�{A���A���A�A�=qA���A���A�Q�A�9XA��#A� �A��/A��^A��`A���A��hA���A�Q�A�/A��9A�TA~ZA~JA{��Aw��Arn�An�HAl�9Ak�Aj��Ah  Agp�Af�/Ae�A`�DA]VA[��AY�AX-AV5?AU�AT�9AS�ARVAP(�AN�AM`BALĜAK��AI�FAG��AEADr�ACA?+A=�^A<�+A:bA7��A6��A5+A3�;A2��A2�A0r�A/�hA/&�A/&�A.�/A.^5A,bNA+�A*Q�A(A�A&r�A%��A%��A%"�A$A�A#\)A"��A"5?A!��A�
A�!AE�A��A��A�#A�TAC�A1'A~�A�PAA�AE�A�+A^5A9XAx�A	��AM�AƨA?}A�+A�7A��AS�Av�A��A33A �j@���@�\)@��!@�$�@��^@�?}@�Ĝ@��w@���@�l�@�\)@��@��@�ff@�p�@���@�I�@�dZ@�v�@�j@�Q�@�C�@���@���@��@���@��@�j@�F@��H@��#@��`@��@��#@�dZ@�\@��H@�|�@��;@��@݉7@�I�@��@�p�@�  @ָR@�\)@���@�1'@�Z@���@Ӆ@�l�@�v�@�?}@�\)@̴9@�{@�{@�@�t�@˥�@�M�@�@��@�p�@�z�@�I�@�"�@ʗ�@ʗ�@�{@���@ȋD@�Q�@�dZ@��H@ư!@�=q@�G�@ě�@�r�@�Q�@Õ�@���@�X@�X@�x�@�%@ă@���@�o@�n�@��T@�p�@�  @���@���@�33@�
=@��R@��H@�+@�33@�dZ@��D@���@��@��@���@�?}@��@�b@�I�@��@�ƨ@�(�@���@�?}@�I�@�ƨ@�S�@�;d@�;d@�o@��@�ȴ@���@���@��@�@��w@���@��j@��`@�ƨ@�x�@�J@��@���@��@���@�ȴ@���@���@�M�@���@��`@�1'@���@��;@��F@��@��w@���@�;d@��y@��y@���@��\@�V@�E�@�E�@��T@��^@��+@��`@�z�@�dZ@���@�dZ@�O�@��m@���@�j@���@�t�@�(�@��@�@�7L@��@��\@�{@�$�@�@�p�@�/@��j@��u@�Z@�Z@�bN@��@��@�C�@�33@�33@�+@�@���@�~�@���@�ȴ@���@��@��@�|�@�E�@���@�`B@���@�Ĝ@��D@�I�@�ƨ@��@��!@�ff@��@���@���@��7@��@�p�@�G�@�/@���@���@��@��@��9@�Q�@�I�@�9X@�(�@��P@�+@���@���@�n�@��@��T@���@���@�@��-@��h@�hs@�X@�?}@��@��9@�z�@��@���@�|�@�K�@��@��H@���@��R@���@�n�@�E�@�-@�@���@�`B@�?}@��@�Z@�1@��m@���@���@�K�@���@�v�@�-@�@��#@���@�`B@�7L@��@��@�r�@�  @�|�@��@��@��\@�5?@���@��^@���@�`B@��@���@��j@���@��u@��@�Z@�1@��w@�S�@��@��\@�~�@�M�@�5?@���@�?}@��@���@���@��D@�9X@�1@��F@�S�@��@�
=@���@�V@�{@��T@���@��7@��@�x�@�x�@�hs@�O�@�/@��@�%@���@��`@��@�b@�1@�P@~�y@}�T@}�-@}�@|��@|z�@|Z@{ƨ@{dZ@z��@z�\@z^5@z-@y��@y�#@yx�@x1'@xb@xb@w�;@w|�@v�R@vV@v5?@v{@u�-@u�@t��@t9X@s��@s�@s33@r��@rM�@qx�@qx�@qG�@q7L@q&�@pĜ@pQ�@p �@o��@oK�@o;d@o
=@nȴ@m�T@mO�@m�@lI�@kƨ@kC�@k@j��@jn�@jM�@jJ@i�#@i��@i��@ihs@h��@g�;@g|�@g;d@g
=@f�y@f�+@fv�@fv�@fv�@fv�@fv�@fV@f5?@f@e��@eO�@d�@d��@d�j@d��@cC�@b=q@bJ@a��@a��@ax�@a%@`  @_|�@_K�@_;d@^�y@^ȴ@^��@^�+@^ff@^@]�T@]O�@\�@\��@\�@\z�@[��@[C�@[o@Z�@Z�\@Y�@Yx�@YG�@Y%@XĜ@X�@W��@W|�@WK�@Vȴ@Vv�@VV@U��@T��@Tj@Tj@TZ@TI�@T(�@S��@SS�@R�\@RM�@R�@Q��@Qx�@Q�@P1'@O��@O|�@Ol�@O�@N��@Nff@N{@M��@M?}@MV@L�@L��@L��@L9X@L(�@L(�@L�@K��@KS�@J�@J~�@J�@JJ@JJ@I��@H��@HĜ@H�9@Hr�@H �@G�w@G+@F��@FE�@E�@E�-@E�@D��@DZ@D�@CdZ@B�@B��@B�!@B��@Bn�@B^5@B=q@B-@B�@A��@Ahs@@�u@@Q�@@b@?��@?�w@?�w@?�@?�P@?|�@?K�@?�@>��@=�@=@=�h@=?}@<�@<�/@<��@<Z@;�
@;�@;"�@:�@:�!@:M�@:J@9��@9��@9�7@9&�@8��@8Ĝ@8�u@8�@8Q�@8A�@8  @8  @7�@7�@7�;@7��@7��@7�@7\)@6�y@6��@6�+@6{@5�h@5V@4�/@4��@4Z@49X@4�@3�m@3�F@3"�@2�!@2~�@2=q@1��@1x�@1X@1G�@17L@1&�@0��@0��@0�`@0Ĝ@0�9@0A�@/�P@/;d@/�@.ȴ@.�+@.5?@.{@-�@-�@-�T@-@-`B@,�/@,(�@,1@+��@+�m@+�F@+�F@+��@+t�@+"�@*��@*��@*�!@*��@*�\@*�\@*n�@*�@)��@)�^@)��@)G�@)%@(�9@(Q�@'�@'|�@&��@&��@&��@&�R@&E�@%��@%�@%`B@%/@$��@$j@$�@#�F@#��@#t�@#"�@"��@"�\@"n�@!�#@!7L@!%@ Ĝ@ Q�@   @�@�@�;@�P@K�@�y@�R@v�@{@��@��@p�@`B@O�@?}@��@�/@�j@�D@Z@9X@��@�
@��@��@��@��@t�@t�@S�@S�@"�@�@�H@��@��@~�@=q@J@��@�@�#@�#@�^@��@��@�7@X@G�@�@��@l�@K�@�@��@ȴ@��@��@v�@V@E�@�-@p�@p�@`B@�@z�@(�@��@��@�m@�F@S�@�H@��@��@-@��@�#@�^@��@x�@G�@7L@&�@%@�`@Ĝ@��@�@bN@1'@ �@�@�;@�;@��@�@�P@�P@|�@l�@K�@+@��@�y@�@ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�;dA�;dA�9XA�33A�7LA�;dA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�7LA�7LA�/A�-A�+A�+A�+A�&�A�&�A�&�A�(�A�(�A�(�A�&�A�(�A�+A�+A�+A�-A�-A�-A�-A�-A�/A�/A�1'A�1'A�1'A�/A�1'A�33A�33A�5?A�7LA�7LA�7LA�7LA�7LA�33A�33A�33A�33A�33A�33A�33A�5?A�/A��TAɶFA�XAƲ-A��
A��;A�VA���A�-A��A���A���A��A���A��A���A�n�A�(�A�ffA�p�A�ƨA���A�O�A��wA�ȴA�\)A�G�A�z�A��wA���A�XA���A��/A���A�n�A��A�-A�1'A�K�A�G�A�%A�{A���A���A�A�=qA���A���A�Q�A�9XA��#A� �A��/A��^A��`A���A��hA���A�Q�A�/A��9A�TA~ZA~JA{��Aw��Arn�An�HAl�9Ak�Aj��Ah  Agp�Af�/Ae�A`�DA]VA[��AY�AX-AV5?AU�AT�9AS�ARVAP(�AN�AM`BALĜAK��AI�FAG��AEADr�ACA?+A=�^A<�+A:bA7��A6��A5+A3�;A2��A2�A0r�A/�hA/&�A/&�A.�/A.^5A,bNA+�A*Q�A(A�A&r�A%��A%��A%"�A$A�A#\)A"��A"5?A!��A�
A�!AE�A��A��A�#A�TAC�A1'A~�A�PAA�AE�A�+A^5A9XAx�A	��AM�AƨA?}A�+A�7A��AS�Av�A��A33A �j@���@�\)@��!@�$�@��^@�?}@�Ĝ@��w@���@�l�@�\)@��@��@�ff@�p�@���@�I�@�dZ@�v�@�j@�Q�@�C�@���@���@��@���@��@�j@�F@��H@��#@��`@��@��#@�dZ@�\@��H@�|�@��;@��@݉7@�I�@��@�p�@�  @ָR@�\)@���@�1'@�Z@���@Ӆ@�l�@�v�@�?}@�\)@̴9@�{@�{@�@�t�@˥�@�M�@�@��@�p�@�z�@�I�@�"�@ʗ�@ʗ�@�{@���@ȋD@�Q�@�dZ@��H@ư!@�=q@�G�@ě�@�r�@�Q�@Õ�@���@�X@�X@�x�@�%@ă@���@�o@�n�@��T@�p�@�  @���@���@�33@�
=@��R@��H@�+@�33@�dZ@��D@���@��@��@���@�?}@��@�b@�I�@��@�ƨ@�(�@���@�?}@�I�@�ƨ@�S�@�;d@�;d@�o@��@�ȴ@���@���@��@�@��w@���@��j@��`@�ƨ@�x�@�J@��@���@��@���@�ȴ@���@���@�M�@���@��`@�1'@���@��;@��F@��@��w@���@�;d@��y@��y@���@��\@�V@�E�@�E�@��T@��^@��+@��`@�z�@�dZ@���@�dZ@�O�@��m@���@�j@���@�t�@�(�@��@�@�7L@��@��\@�{@�$�@�@�p�@�/@��j@��u@�Z@�Z@�bN@��@��@�C�@�33@�33@�+@�@���@�~�@���@�ȴ@���@��@��@�|�@�E�@���@�`B@���@�Ĝ@��D@�I�@�ƨ@��@��!@�ff@��@���@���@��7@��@�p�@�G�@�/@���@���@��@��@��9@�Q�@�I�@�9X@�(�@��P@�+@���@���@�n�@��@��T@���@���@�@��-@��h@�hs@�X@�?}@��@��9@�z�@��@���@�|�@�K�@��@��H@���@��R@���@�n�@�E�@�-@�@���@�`B@�?}@��@�Z@�1@��m@���@���@�K�@���@�v�@�-@�@��#@���@�`B@�7L@��@��@�r�@�  @�|�@��@��@��\@�5?@���@��^@���@�`B@��@���@��j@���@��u@��@�Z@�1@��w@�S�@��@��\@�~�@�M�@�5?@���@�?}@��@���@���@��D@�9X@�1@��F@�S�@��@�
=@���@�V@�{@��T@���@��7@��@�x�@�x�@�hs@�O�@�/@��@�%@���@��`@��@�b@�1@�P@~�y@}�T@}�-@}�@|��@|z�@|Z@{ƨ@{dZ@z��@z�\@z^5@z-@y��@y�#@yx�@x1'@xb@xb@w�;@w|�@v�R@vV@v5?@v{@u�-@u�@t��@t9X@s��@s�@s33@r��@rM�@qx�@qx�@qG�@q7L@q&�@pĜ@pQ�@p �@o��@oK�@o;d@o
=@nȴ@m�T@mO�@m�@lI�@kƨ@kC�@k@j��@jn�@jM�@jJ@i�#@i��@i��@ihs@h��@g�;@g|�@g;d@g
=@f�y@f�+@fv�@fv�@fv�@fv�@fv�@fV@f5?@f@e��@eO�@d�@d��@d�j@d��@cC�@b=q@bJ@a��@a��@ax�@a%@`  @_|�@_K�@_;d@^�y@^ȴ@^��@^�+@^ff@^@]�T@]O�@\�@\��@\�@\z�@[��@[C�@[o@Z�@Z�\@Y�@Yx�@YG�@Y%@XĜ@X�@W��@W|�@WK�@Vȴ@Vv�@VV@U��@T��@Tj@Tj@TZ@TI�@T(�@S��@SS�@R�\@RM�@R�@Q��@Qx�@Q�@P1'@O��@O|�@Ol�@O�@N��@Nff@N{@M��@M?}@MV@L�@L��@L��@L9X@L(�@L(�@L�@K��@KS�@J�@J~�@J�@JJ@JJ@I��@H��@HĜ@H�9@Hr�@H �@G�w@G+@F��@FE�@E�@E�-@E�@D��@DZ@D�@CdZ@B�@B��@B�!@B��@Bn�@B^5@B=q@B-@B�@A��@Ahs@@�u@@Q�@@b@?��@?�w@?�w@?�@?�P@?|�@?K�@?�@>��@=�@=@=�h@=?}@<�@<�/@<��@<Z@;�
@;�@;"�@:�@:�!@:M�@:J@9��@9��@9�7@9&�@8��@8Ĝ@8�u@8�@8Q�@8A�@8  @8  @7�@7�@7�;@7��@7��@7�@7\)@6�y@6��@6�+@6{@5�h@5V@4�/@4��@4Z@49X@4�@3�m@3�F@3"�@2�!@2~�@2=q@1��@1x�@1X@1G�@17L@1&�@0��@0��@0�`@0Ĝ@0�9@0A�@/�P@/;d@/�@.ȴ@.�+@.5?@.{@-�@-�@-�T@-@-`B@,�/@,(�@,1@+��@+�m@+�F@+�F@+��@+t�@+"�@*��@*��@*�!@*��@*�\@*�\@*n�@*�@)��@)�^@)��@)G�@)%@(�9@(Q�@'�@'|�@&��@&��@&��@&�R@&E�@%��@%�@%`B@%/@$��@$j@$�@#�F@#��@#t�@#"�@"��@"�\@"n�@!�#@!7L@!%@ Ĝ@ Q�@   @�@�@�;@�P@K�@�y@�R@v�@{@��@��@p�@`B@O�@?}@��@�/@�j@�D@Z@9X@��@�
@��@��@��@��@t�@t�@S�@S�@"�@�@�H@��@��@~�@=q@J@��@�@�#@�#@�^@��@��@�7@X@G�@�@��@l�@K�@�@��@ȴ@��@��@v�@V@E�@�-@p�@p�@`B@�@z�@(�@��@��@�m@�F@S�@�H@��@��@-@��@�#@�^@��@x�@G�@7L@&�@%@�`@Ĝ@��@�@bN@1'@ �@�@�;@�;@��@�@�P@�P@|�@l�@K�@+@��@�y@�@ȴ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
oB
!�B
�B
�qB?}BO�B^5BbNBgmBm�Bw�B�=B��BĜB�fB��B  BB{BVBB  B��B��B��B�yB�5B��B�jB�{B�Bm�BS�B7LB�BuB
�B
�B
�B
�B
�HB
�B
��B
��B
��B
��B
��B
��B
�RB
�'B
��B
��B
��B
�{B
��B
��B
��B
��B
�DB
�B
x�B
l�B
hsB
YB
@�B
�B
B	��B	�B	�mB	�
B	��B	��B	B	�B	�uB	�JB	�B	x�B	n�B	ffB	dZB	`BB	ZB	N�B	F�B	@�B	;dB	9XB	-B	$�B	�B	�B	oB��B�B�sB�NB��B��B��B��B��B��B��B��B��B��B�
B�#B�5B�B��B�wB�'B�'B�-B�B�B�B�RB�dB�XB�LB�!B�B�B��B��B�B��B�Bw�Bu�Bu�Bo�BjBiyBhsBiyBgmBcTBcTBcTBiyBiyBhsBcTBffBgmBcTBaHBaHBaHBaHBbNBcTBhsBk�Bm�BjBiyBjBjBiyBiyBk�BjBk�Bk�Bl�Bm�Bl�Bl�Bn�Bq�Br�Bu�Bw�Bv�Bx�By�Bz�B}�B�B�uB�oB�VB�uB��B��B��B��B��B��B��B�B�'B�LB�dB�wBŢB��B��B��B��B��BǮB��B�XB�dB��BǮB��BǮBŢB��B�`B�B�B�B�B��B	  B��B��B	  B	B	B	B	B	B	B	B	B		7B	�B	�B	�B	!�B	%�B	,B	2-B	5?B	5?B	49B	;dB	<jB	:^B	:^B	@�B	A�B	A�B	D�B	M�B	P�B	VB	aHB	r�B	y�B	|�B	|�B	z�B	u�B	l�B	ZB	XB	XB	ffB	\)B	ZB	[#B	]/B	_;B	`BB	bNB	dZB	gmB	iyB	n�B	u�B	|�B	�%B	�VB	�\B	�{B	��B	��B	�\B	�{B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�3B	�'B	�'B	�B	�'B	�FB	�-B	�B	�9B	�^B	�RB	�dB	ŢB	B	�}B	�qB	�LB	�-B	�3B	�FB	�FB	�FB	�RB	�dB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�
B	�#B	�BB	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
JB
JB
PB
\B
VB
VB
\B
\B
bB
bB
oB
oB
oB
{B
{B
�B
�B
�B
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
�B
�B
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
!�B
!�B
!�B
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
$�B
$�B
%�B
%�B
%�B
&�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
'�B
'�B
(�B
)�B
)�B
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
-B
-B
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
0!B
0!B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
0!B
1'B
1'B
1'B
2-B
2-B
33B
49B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
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
;dB
;dB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
D�B
D�B
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
J�B
I�B
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
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
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
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
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
dZB
e`B
e`B
e`B
e`B
e`B
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
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
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
p�B
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
v�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
x�B
w�B
w�B
w�B
x�B
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
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
1B
oB
!�B
�B
�qB?}BO�B^5BbNBgmBm�Bw�B�=B��BĜB�fB��B  BB{BVBB  B��B��B��B�yB�5B��B�jB�{B�Bm�BS�B7LB�BuB
�B
�B
�B
�B
�HB
�B
��B
��B
��B
��B
��B
��B
�RB
�'B
��B
��B
��B
�{B
��B
��B
��B
��B
�DB
�B
x�B
l�B
hsB
YB
@�B
�B
B	��B	�B	�mB	�
B	��B	��B	B	�B	�uB	�JB	�B	x�B	n�B	ffB	dZB	`BB	ZB	N�B	F�B	@�B	;dB	9XB	-B	$�B	�B	�B	oB��B�B�sB�NB��B��B��B��B��B��B��B��B��B��B�
B�#B�5B�B��B�wB�'B�'B�-B�B�B�B�RB�dB�XB�LB�!B�B�B��B��B�B��B�Bw�Bu�Bu�Bo�BjBiyBhsBiyBgmBcTBcTBcTBiyBiyBhsBcTBffBgmBcTBaHBaHBaHBaHBbNBcTBhsBk�Bm�BjBiyBjBjBiyBiyBk�BjBk�Bk�Bl�Bm�Bl�Bl�Bn�Bq�Br�Bu�Bw�Bv�Bx�By�Bz�B}�B�B�uB�oB�VB�uB��B��B��B��B��B��B��B�B�'B�LB�dB�wBŢB��B��B��B��B��BǮB��B�XB�dB��BǮB��BǮBŢB��B�`B�B�B�B�B��B	  B��B��B	  B	B	B	B	B	B	B	B	B		7B	�B	�B	�B	!�B	%�B	,B	2-B	5?B	5?B	49B	;dB	<jB	:^B	:^B	@�B	A�B	A�B	D�B	M�B	P�B	VB	aHB	r�B	y�B	|�B	|�B	z�B	u�B	l�B	ZB	XB	XB	ffB	\)B	ZB	[#B	]/B	_;B	`BB	bNB	dZB	gmB	iyB	n�B	u�B	|�B	�%B	�VB	�\B	�{B	��B	��B	�\B	�{B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�3B	�'B	�'B	�B	�'B	�FB	�-B	�B	�9B	�^B	�RB	�dB	ŢB	B	�}B	�qB	�LB	�-B	�3B	�FB	�FB	�FB	�RB	�dB	�wB	�}B	�}B	��B	B	ÖB	ĜB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	�
B	�#B	�BB	�;B	�;B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
JB
JB
PB
\B
VB
VB
\B
\B
bB
bB
oB
oB
oB
{B
{B
�B
�B
�B
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
�B
�B
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
!�B
!�B
!�B
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
$�B
$�B
%�B
%�B
%�B
&�B
%�B
%�B
%�B
&�B
'�B
'�B
(�B
'�B
'�B
(�B
)�B
)�B
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
-B
-B
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
0!B
0!B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
0!B
1'B
1'B
1'B
2-B
2-B
33B
49B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
9XB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
:^B
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
;dB
;dB
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
B�B
C�B
D�B
D�B
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
J�B
I�B
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
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
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
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
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
dZB
e`B
e`B
e`B
e`B
e`B
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
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
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
p�B
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
v�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
x�B
w�B
w�B
w�B
x�B
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
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20200229153742  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200229063753  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200229063754  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200229063754  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200229063755  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200229063755  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200229063755  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200229063755  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200229063756  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200229063756                      G�O�G�O�G�O�                JA  ARUP                                                                        20200229065423                      G�O�G�O�G�O�                