CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-01-08T09:40:33Z creation;2021-01-08T09:40:34Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `x   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ͐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210108094033  20210108095247  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               dA   JA                                  2B  A   APEX                            7906                            051216                          846 @�U�A�1   @�U�@y]@41hr� ��d�=p��
1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CS�fCU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&y�D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�D@��DA� DB  DBy�DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJy�DK  DK� DL  DL� DL��DMy�DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DTy�DU  DU�fDV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\y�D]  D]� D^fD^� D_  D_� D`  D`� D`��Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du� Dv  Dv� DwfDw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{�fD|fD|� D}  D}� D~  D~� D  D�fD�3D�@ D�� D��3D�3D�C3D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D��3D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�<�D D��3D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�3D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ D�|�Dм�D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D��3D�  D�@ Dހ D�� D�  D�C3D߃3D�� D���D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D��D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=q@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�p�B���B���Bģ�B��
Ḅ�B�p�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<8RC>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CT8RCV8RCXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|8RC~Q�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�5�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{DD�D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�{D�D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�{D {D �{D!{D!�{D"{D"�{D#{D#�{D${D$�{D%{D%�{D&D&�D'D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2{D2�{D3{D3�{D4{D4�{D5{D5�{D6{D6�{D7{D7�{D8{D8�{D9{D9�{D:{D:�D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?�{D@{D@�DADA�{DB{DB�DC{DC�{DD{DD�{DE{DE�{DF{DF�DG{DG�{DH{DH�{DI{DI�{DJ{DJ�DK{DK�{DL{DL�{DMDM�DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR{DR�{DS{DS��DT{DT�DU{DU��DV{DV�{DW{DW�DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�D]{D]�{D^�D^�{D_{D_�{D`{D`�{DaDa�{DbDb�{Dc{Dc�{Dd{Dd�{De{De�{Df{Df�{Dg{Dg�{Dh{Dh��Di{Di�{Dj{Dj�{Dk{Dk�{Dl{Dl�{Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt�Dt�{Du{Du�{Dv{Dv�{Dw�Dw�{Dx{Dx�{Dy{Dy�{Dz{Dz�{D{{D{��D|�D|�{D}{D}�{D~{D~�{D{D��D�pD�J=D��=D��pD�pD�MpD��=D��pD�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
D�G
D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��
D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
D�J=D��=D��=D�
=D�J=D��pD��=D�pD�MpD��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�MpD��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��pD�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��pD��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��
D��
D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��pD��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�pD�J=D��
D��=D�
D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�G
D��
D��=D�
=D�J=D��=D��
D�
=D�J=D��=D��=D�
=D�MpD��=D��=D�
=D�J=D��=D��=D�
=D�G
D=D��pD�
=D�J=DÊ=D��=D�
=D�J=DĊ=D��=D�
=D�J=DŊ=D��=D�
=D�J=DƊ=D��=D�
=D�J=DǊ=D��=D�
=D�J=DȊ=D��=D�
=D�J=DɊ=D��=D�
=D�J=Dʊ=D��=D�pD�J=Dˇ
D��=D�
=D�J=D̊=D��=D�
=D�J=D͊=D��=D�
=D�J=DΊ=D��=D�
=D�J=Dϊ=D��=D�
=D�J=DЇ
D��
D�
=D�J=Dъ=D��=D�
=D�J=DҊ=D��=D�
=D�J=Dӊ=D��=D�
=D�J=DԊ=D��=D�
=D�J=DՊ=D��=D�
=D�J=D֊=D��=D�
=D�J=D׊=D��=D�
=D�J=D؊=D��=D�pD�J=Dي=D��=D�
=D�J=Dڊ=D��=D�
=D�J=Dۊ=D��=D�
=D�J=D܊=D��=D�
=D�J=D݊=D��pD�
=D�J=Dފ=D��=D�
=D�MpDߍpD��=D�
D�J=D��=D��=D�
=D�J=D�=D��pD�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�MpD�pD��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��pD�
=D�J=D�=D��
D�
=D�MpD��=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D�=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��=D�
=D�J=D��=D��
D�
=D�J=D��=D��=D�
=D�J=D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��A��HA�ȴAԴ9AԍPA�hsA�dZA�bNA�ZA�O�A�K�A�G�A�E�A�C�A�A�A�=qA�5?A�33A�1'A�1'A�/A�/A�/A�-A�/A�+A�+A�+A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�(�A�(�A�&�A�&�A��A��A�jA϶FA�n�A̓A���A�ffA���AŃA�ȴA§�A�O�A��mA�l�A��A�E�A�`BA�^5A�A�A���A��/A��\A�/A��-A�n�A�XA�ffA��A���A���A�%A���A�ZA�x�A���A�z�A�dZA�ĜA��A��\A��HA�dZA��jA��A���A�
=A�;dA�I�A�C�A�z�A�O�A�1'A�\)A�~�A�t�A���A��FA�n�A�9XA��A��#A�33A�7LA�A��A��-A�ffA�5?A��jA��uA�{A�-A{|�Ax��AwO�Au�AtbArbApbAmVAk�-Ai"�AgS�Ae�mAc�#Ab��Aa��A_�A_A\��AZ �AXE�AVffAU�AR��AR1AQ�wAQ�hAQp�AP��AP9XAM��AK�;AJ��AIp�AF��AE�TADȴAC�mABr�AA�;AA|�A@  A=��A;��A9��A9&�A7�A6��A5dZA4��A49XA2��A1�PA/`BA-|�A,-A+��A*�A*(�A)oA(I�A'"�A&r�A%�mA%C�A$��A$�uA$9XA#?}A!��A!oA E�A��A�A^5A|�A��A;dA�jAAr�A\)A(�AG�AA�A%AbAjA�A��A�FA�9A7LA
�DA	�hA	A�FAbA�!A��AdZA��A�FA�Av�A��AƨA+AVA r�@��h@�`B@�b@���@�(�@�w@���@�t�@홚@웦@�\)@蛦@�5?@�=q@��#@�p�@�P@�~�@�O�@�^5@�\)@���@�p�@�%@���@ؓu@ؓu@�r�@�9X@�|�@�5?@�hs@��@�33@��H@�ff@�{@��#@�x�@��@мj@Ѓ@��m@���@�`B@�1'@�|�@��y@�=q@�@�J@�bN@ǝ�@�+@�ȴ@�-@š�@�&�@��@�1'@�o@�@�M�@�@��@��9@�Q�@���@�33@�ȴ@��@�(�@���@���@��^@���@�X@���@���@���@���@��/@�9X@��@��9@�/@��#@�`B@���@��@�A�@��
@�t�@�\)@��@���@�v�@�5?@��@�O�@���@��`@���@��j@��@� �@���@��P@�dZ@�C�@�o@��y@�^5@�&�@��9@��@�Z@��@��P@�K�@�33@�
=@��\@�5?@�{@�@���@�X@���@���@�Q�@� �@���@��@���@�dZ@�"�@�n�@�{@��@���@�?}@��@��@���@���@�;d@��+@�E�@�M�@�-@���@��T@���@�hs@���@���@�X@�?}@��@�V@�%@���@��@�Ĝ@�z�@� �@�1@�@��@�G�@��@� �@�1'@�Q�@�Z@��@�C�@�33@�+@���@�v�@�ff@�ff@�^5@�=q@��#@�hs@���@�9X@�dZ@���@��R@��R@���@�n�@���@�`B@��j@�z�@� �@�ƨ@�|�@�+@���@�5?@�{@���@���@�x�@�%@��@� �@��@�C�@�C�@�33@��R@�{@��@��#@�@���@�hs@�7L@�V@���@��/@��@��D@��@���@���@���@�|�@�C�@�o@��y@�~�@�=q@�@��@��#@���@�p�@�&�@��j@�b@���@��@�;d@�"�@��y@��+@�=q@��@�@��h@�/@��@���@�Ĝ@��D@�Z@� �@�ƨ@��P@�t�@�S�@�o@���@��@��@���@���@�V@��@���@�X@��@��j@��@�j@�A�@�1@���@��m@��;@���@�ƨ@��w@���@��@�t�@�
=@��@��R@�v�@�E�@���@��^@�X@���@��9@��@�I�@�A�@�A�@� �@�;@�@\)@�@~v�@~$�@}��@}��@}V@|�@{��@z��@z��@z^5@y�@x��@xĜ@x�@xA�@xb@w��@w�@w��@w;d@v�@v��@v��@vv�@u@u�@t�@tI�@t1@s��@s"�@r�H@r�!@q�#@q7L@p��@p��@p�`@p��@p�9@p�9@p��@pr�@p  @o�w@o\)@o
=@n�@nE�@m�@mO�@m/@m�@l�@l��@l�D@l�@k�m@k@k@j��@j-@i�#@i��@ihs@iG�@i%@hĜ@h�u@hQ�@hA�@hA�@h1'@g�@g�@f��@f$�@e@e�-@e�@e?}@e�@d�@d�D@c��@ct�@cC�@c�@c�@c33@c@b�!@bn�@a�#@a7L@`�`@`Ĝ@`��@`�u@`Q�@`b@_�@_��@_�@_l�@_+@^��@^5?@]�T@]p�@]O�@]?}@]/@\�@\��@\I�@[�m@[��@[S�@[o@Z�H@Z��@Zn�@Zn�@Z^5@ZJ@Y�7@YX@X��@X�9@X�u@XQ�@X  @WK�@Vȴ@V��@U�@UO�@Tj@T�@S��@S�F@SS�@R��@R~�@RM�@R�@Q�7@Q7L@P��@PA�@Pb@O�;@O\)@O�@O
=@N��@N�+@Nv�@Nff@N{@M�h@MV@L�D@Lj@L9X@L1@K�
@K��@KdZ@KC�@Ko@K@J��@JJ@I�^@IG�@I�@H��@HĜ@H��@Hb@G��@G|�@F��@FV@F5?@E�@E�-@E�h@E�@D��@D�/@D��@D��@D�@Dj@DI�@Cƨ@CS�@C@B��@B��@Bn�@A��@A�#@Ahs@AX@@Ĝ@@A�@@  @?�@?+@?
=@>ȴ@>��@>ff@=��@=?}@<�@<��@<�@<j@<�@;ƨ@;�F@;�
@;�m@;�
@;ƨ@;�F@;�@;o@:�!@:�\@:��@:�\@9�@9��@9hs@9G�@8�`@8�u@81'@7|�@7�@6�y@6�R@6��@6�R@6�+@6ff@6E�@6@5p�@5?}@4�@4�D@4j@4Z@3�m@3��@3t�@3C�@3@2��@2�\@2^5@2^5@2M�@1�@1�@1��@1��@1x�@1X@1G�@1�@0Ĝ@0bN@0Q�@0b@/��@/K�@.�y@.�+@.E�@.$�@-�@-��@-�-@-V@,��@,Z@,I�@,9X@,9X@,(�@,1@+�
@+@*~�@*^5@*M�@*=q@)��@)��@)hs@)X@)7L@)&�@)%@(�u@(A�@(1'@'�@'�@'
=@&�R@&��@&v�@&ff@&@%��@%�h@%�@%�@%p�@%O�@%/@$��@$��@$�j@$j@$(�@#�m@#�
@#�F@#��@#�@#33@#@#@"�@"�!@"=q@!�@!�^@!��@!��@!x�@!hs@!X@!7L@ ��@ �9@ bN@ b@�;@�;@�w@+@��@�y@�R@�R@��@v�@ff@E�@@�-@�@`B@�@�j@�@�D@I�@9X@(�@�@�m@ƨ@��@t�@33@@�@��@��@�\@=q@�@�@�@�^@��@x�@7L@7L@&�@�@�@��@r�@b@�@l�@+@
=@��@v�@ff@E�@5?@@@@�@O�@�@��@�@��@�D@j@(�@��@��@��@�@t�@t�@t�@S�@33@o@o@�@�!@~�@n�@^5@M�@M�@=q@J@�@�@�#@�^@hs@��@Ĝ@Ĝ@��@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A��A��HA�ȴAԴ9AԍPA�hsA�dZA�bNA�ZA�O�A�K�A�G�A�E�A�C�A�A�A�=qA�5?A�33A�1'A�1'A�/A�/A�/A�-A�/A�+A�+A�+A�(�A�(�A�(�A�+A�+A�+A�+A�+A�+A�+A�(�A�(�A�&�A�&�A��A��A�jA϶FA�n�A̓A���A�ffA���AŃA�ȴA§�A�O�A��mA�l�A��A�E�A�`BA�^5A�A�A���A��/A��\A�/A��-A�n�A�XA�ffA��A���A���A�%A���A�ZA�x�A���A�z�A�dZA�ĜA��A��\A��HA�dZA��jA��A���A�
=A�;dA�I�A�C�A�z�A�O�A�1'A�\)A�~�A�t�A���A��FA�n�A�9XA��A��#A�33A�7LA�A��A��-A�ffA�5?A��jA��uA�{A�-A{|�Ax��AwO�Au�AtbArbApbAmVAk�-Ai"�AgS�Ae�mAc�#Ab��Aa��A_�A_A\��AZ �AXE�AVffAU�AR��AR1AQ�wAQ�hAQp�AP��AP9XAM��AK�;AJ��AIp�AF��AE�TADȴAC�mABr�AA�;AA|�A@  A=��A;��A9��A9&�A7�A6��A5dZA4��A49XA2��A1�PA/`BA-|�A,-A+��A*�A*(�A)oA(I�A'"�A&r�A%�mA%C�A$��A$�uA$9XA#?}A!��A!oA E�A��A�A^5A|�A��A;dA�jAAr�A\)A(�AG�AA�A%AbAjA�A��A�FA�9A7LA
�DA	�hA	A�FAbA�!A��AdZA��A�FA�Av�A��AƨA+AVA r�@��h@�`B@�b@���@�(�@�w@���@�t�@홚@웦@�\)@蛦@�5?@�=q@��#@�p�@�P@�~�@�O�@�^5@�\)@���@�p�@�%@���@ؓu@ؓu@�r�@�9X@�|�@�5?@�hs@��@�33@��H@�ff@�{@��#@�x�@��@мj@Ѓ@��m@���@�`B@�1'@�|�@��y@�=q@�@�J@�bN@ǝ�@�+@�ȴ@�-@š�@�&�@��@�1'@�o@�@�M�@�@��@��9@�Q�@���@�33@�ȴ@��@�(�@���@���@��^@���@�X@���@���@���@���@��/@�9X@��@��9@�/@��#@�`B@���@��@�A�@��
@�t�@�\)@��@���@�v�@�5?@��@�O�@���@��`@���@��j@��@� �@���@��P@�dZ@�C�@�o@��y@�^5@�&�@��9@��@�Z@��@��P@�K�@�33@�
=@��\@�5?@�{@�@���@�X@���@���@�Q�@� �@���@��@���@�dZ@�"�@�n�@�{@��@���@�?}@��@��@���@���@�;d@��+@�E�@�M�@�-@���@��T@���@�hs@���@���@�X@�?}@��@�V@�%@���@��@�Ĝ@�z�@� �@�1@�@��@�G�@��@� �@�1'@�Q�@�Z@��@�C�@�33@�+@���@�v�@�ff@�ff@�^5@�=q@��#@�hs@���@�9X@�dZ@���@��R@��R@���@�n�@���@�`B@��j@�z�@� �@�ƨ@�|�@�+@���@�5?@�{@���@���@�x�@�%@��@� �@��@�C�@�C�@�33@��R@�{@��@��#@�@���@�hs@�7L@�V@���@��/@��@��D@��@���@���@���@�|�@�C�@�o@��y@�~�@�=q@�@��@��#@���@�p�@�&�@��j@�b@���@��@�;d@�"�@��y@��+@�=q@��@�@��h@�/@��@���@�Ĝ@��D@�Z@� �@�ƨ@��P@�t�@�S�@�o@���@��@��@���@���@�V@��@���@�X@��@��j@��@�j@�A�@�1@���@��m@��;@���@�ƨ@��w@���@��@�t�@�
=@��@��R@�v�@�E�@���@��^@�X@���@��9@��@�I�@�A�@�A�@� �@�;@�@\)@�@~v�@~$�@}��@}��@}V@|�@{��@z��@z��@z^5@y�@x��@xĜ@x�@xA�@xb@w��@w�@w��@w;d@v�@v��@v��@vv�@u@u�@t�@tI�@t1@s��@s"�@r�H@r�!@q�#@q7L@p��@p��@p�`@p��@p�9@p�9@p��@pr�@p  @o�w@o\)@o
=@n�@nE�@m�@mO�@m/@m�@l�@l��@l�D@l�@k�m@k@k@j��@j-@i�#@i��@ihs@iG�@i%@hĜ@h�u@hQ�@hA�@hA�@h1'@g�@g�@f��@f$�@e@e�-@e�@e?}@e�@d�@d�D@c��@ct�@cC�@c�@c�@c33@c@b�!@bn�@a�#@a7L@`�`@`Ĝ@`��@`�u@`Q�@`b@_�@_��@_�@_l�@_+@^��@^5?@]�T@]p�@]O�@]?}@]/@\�@\��@\I�@[�m@[��@[S�@[o@Z�H@Z��@Zn�@Zn�@Z^5@ZJ@Y�7@YX@X��@X�9@X�u@XQ�@X  @WK�@Vȴ@V��@U�@UO�@Tj@T�@S��@S�F@SS�@R��@R~�@RM�@R�@Q�7@Q7L@P��@PA�@Pb@O�;@O\)@O�@O
=@N��@N�+@Nv�@Nff@N{@M�h@MV@L�D@Lj@L9X@L1@K�
@K��@KdZ@KC�@Ko@K@J��@JJ@I�^@IG�@I�@H��@HĜ@H��@Hb@G��@G|�@F��@FV@F5?@E�@E�-@E�h@E�@D��@D�/@D��@D��@D�@Dj@DI�@Cƨ@CS�@C@B��@B��@Bn�@A��@A�#@Ahs@AX@@Ĝ@@A�@@  @?�@?+@?
=@>ȴ@>��@>ff@=��@=?}@<�@<��@<�@<j@<�@;ƨ@;�F@;�
@;�m@;�
@;ƨ@;�F@;�@;o@:�!@:�\@:��@:�\@9�@9��@9hs@9G�@8�`@8�u@81'@7|�@7�@6�y@6�R@6��@6�R@6�+@6ff@6E�@6@5p�@5?}@4�@4�D@4j@4Z@3�m@3��@3t�@3C�@3@2��@2�\@2^5@2^5@2M�@1�@1�@1��@1��@1x�@1X@1G�@1�@0Ĝ@0bN@0Q�@0b@/��@/K�@.�y@.�+@.E�@.$�@-�@-��@-�-@-V@,��@,Z@,I�@,9X@,9X@,(�@,1@+�
@+@*~�@*^5@*M�@*=q@)��@)��@)hs@)X@)7L@)&�@)%@(�u@(A�@(1'@'�@'�@'
=@&�R@&��@&v�@&ff@&@%��@%�h@%�@%�@%p�@%O�@%/@$��@$��@$�j@$j@$(�@#�m@#�
@#�F@#��@#�@#33@#@#@"�@"�!@"=q@!�@!�^@!��@!��@!x�@!hs@!X@!7L@ ��@ �9@ bN@ b@�;@�;@�w@+@��@�y@�R@�R@��@v�@ff@E�@@�-@�@`B@�@�j@�@�D@I�@9X@(�@�@�m@ƨ@��@t�@33@@�@��@��@�\@=q@�@�@�@�^@��@x�@7L@7L@&�@�@�@��@r�@b@�@l�@+@
=@��@v�@ff@E�@5?@@@@�@O�@�@��@�@��@�D@j@(�@��@��@��@�@t�@t�@t�@S�@33@o@o@�@�!@~�@n�@^5@M�@M�@=q@J@�@�@�#@�^@hs@��@Ĝ@Ĝ@��@�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bs�Bs�Bs�Bt�Bu�Bv�Bx�By�B{�B|�B|�B|�B|�B|�B|�B}�B}�B}�B}�B}�B}�B}�B~�B~�B~�B~�B~�B~�B~�B� B� B� B� B� B� B�B�B�B�B�B�B�B�B�%B�uB�sB$�B7LB@�BI�BL�B8RBuBBbB>wBI�BVBW
BW
BM�BH�BE�BK�B[#B]/B\)B^5B^5BbNBm�BgmB`BBXBP�BJ�BD�B>wB=qBC�B33B�B�B�BbBJBB�B�sB�;B�
BĜB�^B�B��B�bB�+B�Bu�Be`BS�BO�BK�BH�BC�B6FB\B
��B
�B
�yB
�TB
�
B
�?B
�{B
� B
bNB
L�B
9XB
0!B
%�B
�B
\B
B	�B	�yB	�#B	��B	ǮB	�qB	�9B	�!B	��B	��B	��B	�1B	~�B	s�B	m�B	_;B	ZB	XB	VB	T�B	Q�B	N�B	B�B	7LB	0!B	)�B	 �B	�B	{B	bB		7B	%B	1B	B	  B��B�B�B�B�B�BB�ZB�TB�/B�B�B��BƨBŢBB�}B�jB�XB�FB�3B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�1B�B~�B}�B}�B� Bz�B� B�7B�B�Bz�Bw�Bs�Br�Bq�B�B�{B�uB�oB��B��B�{B��B��B��B��B��B��B��B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B��B��B�B�'B�XB�^B�dB�wB�wB�wB�dB�XB�^B�dB�qB��B��B��BÖBĜBŢBŢBǮB��B��B��B��B�B�5B�NB�fB�ZB�fB�mB�yB�B�B�B��B��B��B��B��B	  B	+B	PB	hB	�B	�B	�B	�B	"�B	&�B	(�B	+B	,B	1'B	:^B	?}B	A�B	C�B	F�B	I�B	H�B	P�B	T�B	[#B	aHB	e`B	hsB	k�B	m�B	p�B	q�B	t�B	w�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�DB	�JB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�3B	�?B	�LB	�RB	�XB	�^B	�^B	�jB	�qB	�}B	��B	B	ŢB	ǮB	ǮB	ȴB	ǮB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�;B	�5B	�;B	�;B	�HB	�ZB	�ZB	�fB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B

=B

=B

=B

=B

=B
DB
JB
PB
PB
PB
PB
PB
VB
VB
\B
\B
bB
bB
bB
\B
hB
hB
hB
hB
hB
hB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
&�B
'�B
&�B
$�B
$�B
$�B
$�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
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
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
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
5?B
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
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
;dB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
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
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
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
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
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
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
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
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
bNB
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
gmB
gmB
gmB
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
m�B
m�B
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
r�B
r�B
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
w�B
v�B
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
x�B
x�B
x�B
x�B
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
~�B
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
~�B
~�B
� B
� B
� B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Bs�Bs�Bs�Bt�Bu�Bv�Bx�By�B{�B|�B|�B|�B|�B|�B|�B}�B}�B}�B}�B}�B}�B}�B~�B~�B~�B~�B~�B~�B~�B� B� B� B� B� B� B�B�B�B�B�B�B�B�B�%B�uB�sB$�B7LB@�BI�BL�B8RBuBBbB>wBI�BVBW
BW
BM�BH�BE�BK�B[#B]/B\)B^5B^5BbNBm�BgmB`BBXBP�BJ�BD�B>wB=qBC�B33B�B�B�BbBJBB�B�sB�;B�
BĜB�^B�B��B�bB�+B�Bu�Be`BS�BO�BK�BH�BC�B6FB\B
��B
�B
�yB
�TB
�
B
�?B
�{B
� B
bNB
L�B
9XB
0!B
%�B
�B
\B
B	�B	�yB	�#B	��B	ǮB	�qB	�9B	�!B	��B	��B	��B	�1B	~�B	s�B	m�B	_;B	ZB	XB	VB	T�B	Q�B	N�B	B�B	7LB	0!B	)�B	 �B	�B	{B	bB		7B	%B	1B	B	  B��B�B�B�B�B�BB�ZB�TB�/B�B�B��BƨBŢBB�}B�jB�XB�FB�3B�!B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�1B�B~�B}�B}�B� Bz�B� B�7B�B�Bz�Bw�Bs�Br�Bq�B�B�{B�uB�oB��B��B�{B��B��B��B��B��B��B��B�bB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B�B��B��B�B�'B�XB�^B�dB�wB�wB�wB�dB�XB�^B�dB�qB��B��B��BÖBĜBŢBŢBǮB��B��B��B��B�B�5B�NB�fB�ZB�fB�mB�yB�B�B�B��B��B��B��B��B	  B	+B	PB	hB	�B	�B	�B	�B	"�B	&�B	(�B	+B	,B	1'B	:^B	?}B	A�B	C�B	F�B	I�B	H�B	P�B	T�B	[#B	aHB	e`B	hsB	k�B	m�B	p�B	q�B	t�B	w�B	y�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�DB	�JB	�PB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�3B	�?B	�LB	�RB	�XB	�^B	�^B	�jB	�qB	�}B	��B	B	ŢB	ǮB	ǮB	ȴB	ǮB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	�B	�B	�#B	�/B	�5B	�5B	�;B	�;B	�;B	�BB	�HB	�;B	�5B	�;B	�;B	�HB	�ZB	�ZB	�fB	�fB	�fB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
+B
1B
	7B
	7B

=B

=B

=B

=B

=B
DB
JB
PB
PB
PB
PB
PB
VB
VB
\B
\B
bB
bB
bB
\B
hB
hB
hB
hB
hB
hB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
'�B
&�B
'�B
&�B
$�B
$�B
$�B
$�B
&�B
&�B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
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
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
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
5?B
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
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
;dB
=qB
>wB
>wB
>wB
?}B
?}B
@�B
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
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
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
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
R�B
S�B
S�B
S�B
S�B
S�B
T�B
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
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
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
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
bNB
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
gmB
gmB
gmB
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
m�B
m�B
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
r�B
r�B
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
w�B
v�B
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
x�B
x�B
x�B
x�B
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
~�B
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
~�B
~�B
� B
� B
� B
~�B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20210108154912  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210108094033  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210108094033  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210108094033  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210108094034  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210108094034  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210108094034  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210108094034  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210108094034  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210108094034                      G�O�G�O�G�O�                JA  ARUP                                                                        20210108095247                      G�O�G�O�G�O�                