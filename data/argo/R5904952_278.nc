CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:09Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190609  20181005190609  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���*��1   @���8� @2vȴ9X�c�;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     A   A   B   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C  C  C  C  C  C�fC"  C$�C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CQ�fCS�fCV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  Dy�D��D� D  D� DfD�fDfD� D  D� D  D� D  D� D  D� D  D� D��D� DfD�fD  D� D  Dy�D��Dy�D  D� D  D� D��D� D  D� D��D� D   D y�D ��D!� D"  D"y�D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2�fD3  D3� D4  D4� D4��D5� D6  D6� D6��D7y�D8  D8y�D9  D9� D:  D:� D:��D;y�D<  D<� D<��D=� D>  D>� D?  D?�fD@fD@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJ�fDK  DK� DK��DLy�DM  DM� DN  DN�fDO  DOy�DPfDP� DQ  DQ�fDQ��DR� DSfDS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DY��DZy�D[  D[�fD\fD\�fD]  D]� D^  D^� D_  D_y�Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv��Dw� Dw��Dy��D�B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @AG�@�=q@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�Bh�HBqG�ByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B��
B���B���Bģ�Bȣ�B��
BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�CQ�C8RCQ�CQ�CQ�CQ�CQ�CQ�CQ�C 8RC"Q�C$k�C&k�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPk�CR8RCT8RCVQ�CXQ�CZQ�C\Q�C^Q�C`k�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�CvQ�CxQ�CzQ�C|Q�C~Q�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�)C�)C�)C�)C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�)C�(�C�5�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�)C�(�C�5�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�5�C�(�D {D �{D{D�{D{D�{D{D�{D{D�{D{D�{D{D��D{D�{D{D�{D	{D	�{D
{D
�{D{D�{D{D�DD�{D{D�{D�D��D�D�{D{D�{D{D�{D{D�{D{D�{D{D�{DD�{D�D��D{D�{D{D�DD�D{D�{D{D�{DD�{D{D�{DD�{D {D �D!D!�{D"{D"�D#{D#��D${D$�{D%{D%�{D&{D&�{D'{D'�{D({D(�{D){D)�{D*{D*�{D+{D+�{D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2�D2��D3{D3�{D4{D4�{D5D5�{D6{D6�{D7D7�D8{D8�D9{D9�{D:{D:�{D;D;�D<{D<�{D=D=�{D>{D>�{D?{D?��D@�D@�{DA{DA�{DB{DB�{DCDC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�DH{DH�{DI{DI�{DJ{DJ��DK{DK�{DLDL�DM{DM�{DN{DN��DO{DO�DP�DP�{DQ{DQ��DRDR�{DS�DS�{DT{DT�{DU�DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZDZ�D[{D[��D\�D\��D]{D]�{D^{D^�{D_{D_�Dk�{Dl{Dl�Dm{Dm�{Dn{Dn�{Do{Do�{Dp{Dp�{Dq{Dq�{Dr{Dr�{Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{DwDw�{DxHDy�\D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�33A�5?A�7LA�7LA�7LA�9XA�;dA�9XA�=qA�;dA�-A�%A���A���A�ȴA���A���A��
A��#A��TA��`A��`A��`A��TA��;A��
A���A�r�A���A��AƃA�G�A�
=Aŧ�A�  AĲ-A�1A���Aß�A�Q�A�I�A�1'AuA��A���A��RA��-A���A�r�A�;dA��#A��RA��uA�M�A��jA���A��uA�S�A�oA��#A��-A�t�A��HA�hsA�;dA��#A��9A���A�/A�\)A��HA�1'A�x�A�E�A��`A�7LA�A��PA�^5A��A���A�t�A�O�A�M�A��A�VA�VA��7A�$�A�G�A��FA�dZA�  A��+A��PA��A��A�\)A��A���A���A��uA��A��9A�x�A���A�C�A���A���A��A�ȴA���A�dZA��A�JA� �A���A}�TA{oAv{Ar=qAoAmAiG�Aa�A[�AR��AM�
AM"�AK�AIXAG��AF1'AE&�AC��AA\)A??}A>-A<�A:ĜA8��A4�`A0Q�A.�\A-�-A,��A+G�A)%A(ZA(1A'��A'|�A'`BA'O�A&1A$=qA"r�A"-A!+A ��A �A�A�wA��A�mAoA��A��AbAĜA/A$�A�#A�-A�/AG�A��A�mA?}A��A��AA�AbA�7A�A��A1'A��A�A��A�A
��A	l�A9XA��A�7A&�A�!AE�Al�A{A|�A�+A�A��A�/A�hAO�AoA ��@�dZ@��@�M�@�Ĝ@��y@�ȴ@�
=@�\)@���@���@��7@�G�@��;@�n�@�x�@�V@���@���@���@�j@�Z@��;@�ƨ@�|�@�R@�J@��-@���@홚@�@�p�@�O�@�7L@�Ĝ@�Q�@�ƨ@�S�@�\@���@�D@�V@���@���@��@��D@�(�@߅@ݲ-@��H@�b@��@��@���@��
@�|�@��@ޗ�@��@�x�@��@�z�@ۥ�@�dZ@�K�@ڏ\@�-@�{@�{@��#@ف@��@׾w@��@�b@ҟ�@�E�@щ7@��@�+@��T@��;@�;d@˶F@� �@�bN@��@�-@�l�@�@�\)@���@�1@�=q@���@�9X@̃@�V@��@��@��@̣�@���@�33@�
=@ȣ�@�ff@�`B@�=q@�t�@�\)@��y@�I�@�O�@��@�X@��@��P@���@�S�@�1@�1'@�Q�@�r�@��u@���@�r�@���@��;@��@��!@���@��+@�?}@�"�@�ff@��@���@��h@�O�@��j@� �@��@�;d@�dZ@��y@���@���@��
@��@���@�S�@�@���@�?}@�V@��@��j@�b@��@���@���@���@��\@�n�@�V@��!@�|�@�I�@��!@�K�@�ȴ@�J@�p�@�?}@���@�r�@�b@��@��R@�E�@��T@�J@�5?@�=q@�G�@�bN@��;@��F@�l�@�o@���@��+@�{@�p�@��@���@�bN@��@��P@�33@���@���@�^5@�V@�$�@��@���@��@�9X@�(�@�ƨ@�K�@�+@��y@��\@�v�@�ff@�5?@�@���@��@��9@�r�@�j@�j@�z�@�z�@�A�@���@���@�C�@��@��H@���@�v�@��\@��`@�bN@���@�t�@�l�@�dZ@�S�@��@���@���@���@��@�G�@�%@���@�z�@�Q�@�A�@�A�@�(�@� �@�1@��@�t�@�K�@�33@��;@�>B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�5?A�33A�5?A�7LA�7LA�7LA�9XA�;dA�9XA�=qA�;dA�-A�%A���A���A�ȴA���A���A��
A��#A��TA��`A��`A��`A��TA��;A��
A���A�r�A���A��AƃA�G�A�
=Aŧ�A�  AĲ-A�1A���Aß�A�Q�A�I�A�1'AuA��A���A��RA��-A���A�r�A�;dA��#A��RA��uA�M�A��jA���A��uA�S�A�oA��#A��-A�t�A��HA�hsA�;dA��#A��9A���A�/A�\)A��HA�1'A�x�A�E�A��`A�7LA�A��PA�^5A��A���A�t�A�O�A�M�A��A�VA�VA��7A�$�A�G�A��FA�dZA�  A��+A��PA��A��A�\)A��A���A���A��uA��A��9A�x�A���A�C�A���A���A��A�ȴA���A�dZA��A�JA� �A���A}�TA{oAv{Ar=qAoAmAiG�Aa�A[�AR��AM�
AM"�AK�AIXAG��AF1'AE&�AC��AA\)A??}A>-A<�A:ĜA8��A4�`A0Q�A.�\A-�-A,��A+G�A)%A(ZA(1A'��A'|�A'`BA'O�A&1A$=qA"r�A"-A!+A ��A �A�A�wA��A�mAoA��A��AbAĜA/A$�A�#A�-A�/AG�A��A�mA?}A��A��AA�AbA�7A�A��A1'A��A�A��A�A
��A	l�A9XA��A�7A&�A�!AE�Al�A{A|�A�+A�A��A�/A�hAO�AoA ��@�dZ@��@�M�@�Ĝ@��y@�ȴ@�
=@�\)@���@���@��7@�G�@��;@�n�@�x�@�V@���@���@���@�j@�Z@��;@�ƨ@�|�@�R@�J@��-@���@홚@�@�p�@�O�@�7L@�Ĝ@�Q�@�ƨ@�S�@�\@���@�D@�V@���@���@��@��D@�(�@߅@ݲ-@��H@�b@��@��@���@��
@�|�@��@ޗ�@��@�x�@��@�z�@ۥ�@�dZ@�K�@ڏ\@�-@�{@�{@��#@ف@��@׾w@��@�b@ҟ�@�E�@щ7@��@�+@��T@��;@�;d@˶F@� �@�bN@��@�-@�l�@�@�\)@���@�1@�=q@���@�9X@̃@�V@��@��@��@̣�@���@�33@�
=@ȣ�@�ff@�`B@�=q@�t�@�\)@��y@�I�@�O�@��@�X@��@��P@���@�S�@�1@�1'@�Q�@�r�@��u@���@�r�@���@��;@��@��!@���@��+@�?}@�"�@�ff@��@���@��h@�O�@��j@� �@��@�;d@�dZ@��y@���@���@��
@��@���@�S�@�@���@�?}@�V@��@��j@�b@��@���@���@���@��\@�n�@�V@��!@�|�@�I�@��!@�K�@�ȴ@�J@�p�@�?}@���@�r�@�b@��@��R@�E�@��T@�J@�5?@�=q@�G�@�bN@��;@��F@�l�@�o@���@��+@�{@�p�@��@���@�bN@��@��P@�33@���@���@�^5@�V@�$�@��@���@��@�9X@�(�@�ƨ@�K�@�+@��y@��\@�v�@�ff@�5?@�@���@��@��9@�r�@�j@�j@�z�@�z�@�A�@���@���@�C�@��@��H@���@�v�@��\@��`@�bN@���@�t�@�l�@�dZ@�S�@��@���@���@���@��@�G�@�%@���@�z�@�Q�@�A�@�A�@�(�@� �@�1@��@�t�@�K�@�33@��;@�>B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B�B�B��B�
B�#B�ZB�B��B	%B	�B	'�B	5?B	<jB	P�B	]/B	k�B	u�B	� B	�+B	�\B	��B	�fB
C�B
v�B
�B
�bB
��B
��B
ƨB
��B
��B
�mB
�B
��BBDB�B�B$�B&�B&�B'�B2-B@�BD�BE�BE�BH�BYB]/B`BBe`Bm�Bv�B�B�=B��B��B��B�FB�jBB�)BB�B5?BB�BE�BJ�BYBcTBiyBl�Bq�Bv�B|�B� B� B�B�B�bB��B��B��B�1B�DB�JB|�B\)BH�B=qB>wBE�B'�B�BB�B�ZB�9Bv�BG�B$�BDBB
��B
��B
{�B
aHB
R�B
oB	�5B	��B	�DB	`BB	@�B	49B	)�B	�B	+B�B��B��B��B��B��BȴBǮBŢBÖB��BĜBŢBŢBɺB��B��BȴBȴBȴB��B��B��B��BɺBɺBɺBɺBȴB��B��B��B��B��B��B��B��B�B�B�/B�HB�NB�TB�TB�NB�#B�B�#B�NB�`B�ZB�B�B��B��B	B	B	B	  B	B	B	�B	;dB	P�B	N�B	H�B	E�B	B�B	?}B	9XB	33B	33B	0!B	,B	-B	+B	'�B	8RB	=qB	@�B	I�B	N�B	N�B	L�B	I�B	E�B	B�B	B�B	B�B	A�B	B�B	D�B	K�B	T�B	ZB	YB	\)B	^5B	]/B	_;B	`BB	`BB	`BB	`BB	`BB	`BB	cTB	dZB	ffB	hsB	jB	k�B	k�B	k�B	k�B	l�B	m�B	m�B	q�B	t�B	x�B	{�B	�B	� B	|�B	u�B	k�B	iyB	jB	k�B	jB	jB	n�B	}�B	�1B	�1B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�XB	�9B	�B	��B	��B	��B	��B	��B	��B	�bB	�\B	�{B	��B	��B	��B	�oB	�+B	�B	|�B	�=B	��B	�B	�RB	�jB	��B	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ǮB	ǮB	��B	�qB	�dB	��B	ǮB	ɺB	ǮB	�jB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�!B	�!B	�!B	�!B	�!B	�-B	�?B	�LB	�RB	�RB	�XB	�^B	�qB	��B	ǮB	��B	�#B	�BB	�;B	�5B	�)B	�)B	�/B	�;B	�;B	�BB	�BB	�BB	�HB	�ZB	�fB	�fB	�sB	�sB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
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
B
B
BVB
DB

=B
DB
DB
DB
DB
DB
DB

=B
JB
VB
VB
PB
PB
PB
JB
JB
JB
JB
JB
JB
DB
DB

=B

=B
DB

=B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222 B��B��B��B��B��B��B��B�B�B��B�
B�#B�ZB�B��B	%B	�B	'�B	5?B	<jB	P�B	]/B	k�B	u�B	� B	�+B	�\B	��B	�fB
C�B
v�B
�B
�bB
��B
��B
ƨB
��B
��B
�mB
�B
��BBDB�B�B$�B&�B&�B'�B2-B@�BD�BE�BE�BH�BYB]/B`BBe`Bm�Bv�B�B�=B��B��B��B�FB�jBB�)BB�B5?BB�BE�BJ�BYBcTBiyBl�Bq�Bv�B|�B� B� B�B�B�bB��B��B��B�1B�DB�JB|�B\)BH�B=qB>wBE�B'�B�BB�B�ZB�9Bv�BG�B$�BDBB
��B
��B
{�B
aHB
R�B
oB	�5B	��B	�DB	`BB	@�B	49B	)�B	�B	+B�B��B��B��B��B��BȴBǮBŢBÖB��BĜBŢBŢBɺB��B��BȴBȴBȴB��B��B��B��BɺBɺBɺBɺBȴB��B��B��B��B��B��B��B��B�B�B�/B�HB�NB�TB�TB�NB�#B�B�#B�NB�`B�ZB�B�B��B��B	B	B	B	  B	B	B	�B	;dB	P�B	N�B	H�B	E�B	B�B	?}B	9XB	33B	33B	0!B	,B	-B	+B	'�B	8RB	=qB	@�B	I�B	N�B	N�B	L�B	I�B	E�B	B�B	B�B	B�B	A�B	B�B	D�B	K�B	T�B	ZB	YB	\)B	^5B	]/B	_;B	`BB	`BB	`BB	`BB	`BB	`BB	cTB	dZB	ffB	hsB	jB	k�B	k�B	k�B	k�B	l�B	m�B	m�B	q�B	t�B	x�B	{�B	�B	� B	|�B	u�B	k�B	iyB	jB	k�B	jB	jB	n�B	}�B	�1B	�1B	�DB	�VB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�9B	�XB	�9B	�B	��B	��B	��B	��B	��B	��B	�bB	�\B	�{B	��B	��B	��B	�oB	�+B	�B	|�B	�=B	��B	�B	�RB	�jB	��B	ƨB	ǮB	ǮB	ǮB	ǮB	ȴB	ǮB	ǮB	��B	�qB	�dB	��B	ǮB	ɺB	ǮB	�jB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�!B	�!B	�!B	�!B	�!B	�-B	�?B	�LB	�RB	�RB	�XB	�^B	�qB	��B	ǮB	��B	�#B	�BB	�;B	�5B	�)B	�)B	�/B	�;B	�;B	�BB	�BB	�BB	�HB	�ZB	�fB	�fB	�sB	�sB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
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
B
B
BVB
DB

=B
DB
DB
DB
DB
DB
DB

=B
JB
VB
VB
PB
PB
PB
JB
JB
JB
JB
JB
JB
DB
DB

=B

=B
DB

=B
�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190609                              AO  ARCAADJP                                                                    20181005190609    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190609  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190609  QCF$                G�O�G�O�G�O�8000            