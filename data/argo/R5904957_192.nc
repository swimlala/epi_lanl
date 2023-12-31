CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:40Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140840  20181024140840  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�ߤԓ9C1   @�ߥffy@5Y������dM���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A��A   A>ffA`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C  C  C  C  C  C�C  C   C"  C#�fC&  C(  C*  C,  C.  C/�fC1�fC4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL�CN�CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C{�fC~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fDfD�fD  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D��D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  Dy�D  D� D   D y�D!  D!� D"  D"�fD#fD#� D$  D$� D%  D%� D&fD&� D'  D'�fD(  D(y�D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D1��D2� D3fD3� D4  D4� D5  D5�fD6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@fD@� D@��DAy�DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� D`��Day�Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df�fDgfDg� Dh  Dh�fDifDi� DjfDj�fDk  Dk�fDlfDl�fDmfDm�fDnfDn� Do  Do� Do��Dp� Dq  Dq� Dq��Dry�Ds  Ds�fDtfDt� Dt��Duy�Dv  Dv� Dw  Dw� Dw��Dy�qD�2�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�p�@�=qA�RA%�AC�Ae�A�\)A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B���B���B���B���B��
B���B���B���B���B���B���B�p�B���B���B���B���B���Bģ�B��
Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
8RCQ�CQ�CQ�CQ�CQ�CQ�CQ�CQ�Ck�CQ�C Q�C"Q�C$8RC&Q�C(Q�C*Q�C,Q�C.Q�C08RC28RC4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBk�CDQ�CFQ�CHQ�CJQ�CLk�CNk�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`8RCbQ�CdQ�Cf8RChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�Cvk�CxQ�CzQ�C|8RC~Q�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�5�C�5�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�5�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�(�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�(�D {D �{D{D�{D{D�{D{D�D{D�{D{D�{D{D�{D{D�{D{D�{D	{D	�{D
{D
��D�D��D{D�{D{D�{D�D�{D{D�{D{D�{D{D�{D{D�{D{D�{D{D�D{D�{D{D�{DD�{D{D�{D{D�{D{D�D{D�{D{D�{D{D�{D{D�D{D�{D {D �D!{D!�{D"{D"��D#�D#�{D${D$�{D%{D%�{D&�D&�{D'{D'��D({D(�D){D)�{D*{D*�{D+{D+��D,{D,�{D-{D-�{D.{D.�{D/{D/�{D0{D0�{D1{D1�{D2D2�{D3�D3�{D4{D4�{D5{D5��D6{D6�{D7{D7��D8{D8�{D9{D9�{D:{D:�{D;{D;�{D<{D<�{D={D=�{D>{D>�{D?{D?��D@�D@�{DADA�DB{DB�{DC{DC�{DD{DD�{DE{DE�{DF{DF�{DG{DG�{DH{DH�{DI{DI��DJ{DJ�{DK{DK�{DL{DL�{DM{DM�{DN{DN�{DO{DO�{DP{DP�{DQ{DQ�{DR�DR�{DS{DS�{DT{DT�{DU{DU�{DV{DV�{DW{DW�{DX{DX�{DY{DY�{DZ{DZ�{D[{D[�{D\{D\�{D]{D]�{D^{D^�{D_{D_�{D`{D`�{DaDa�Db{Db�{Dc{Dc�{DdDd�{De{De�{Df{Df��Dg�Dg�{Dh{Dh��Di�Di�{Dj�Dj��Dk{Dk��Dl�Dl��Dm�Dm��Dn�Dn�{Do{Do�{DpDp�{Dq{Dq�{DrDr�Ds{Ds��Dt�Dt�{DuDu�Dv{Dv�{Dw{Dw�{DxHDy��D�=D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�dZA�dZA�dZA�dZA�bNA�bNA�VA�7LA��A���A��#A�  A�"�A�7LA�l�AϋDAω7AσA�jA�XA�?}A��A�ZA��A���A��#A�AʾwA��A�$�A�/A�"�Aé�AÕ�A�33A�l�A�(�A�ȴA�x�A�bA���A��A�7LA�1'A���A��jA�$�A���A��yA�;dA�XA��A�VA�ffA��
A�&�A���A��A��HA�{A�VA��!A��7A��+A��PA�"�A�C�A��`A�ZA�A��A���A���A�
=A�
=A��`A�hsA�G�A�ZA���A�^5A�r�A��A�S�A��RA��A�1'A�~�A�A�A�A�E�A�ȴA�M�A��A~z�A|�jA{\)AzQ�AxjAv��Au�At�jAs�PAr{Aq�Ao�AmS�Ak�#Ah�Agt�AfM�AdQ�AcC�A^�jA[G�AY��AW�TAU�mATr�AS�#AS�AQ�AQ%AP9XANZAK\)AJAI|�AI+AH��AH(�AE�wAB�DA>�yA=�A:��A7A4$�A4  A3�A3A3�7A2��A1�-A1VA0�\A.�A-;dA,bNA+�mA*$�A(ffA&ZA$�9A${A#
=A"��A!t�A �\A�;A�7A�+A{Ap�AoA�A�A�
A|�A+A�A&�AbAA��A�A�A33A�A�AVAG�AȴAv�A�A�FA�mA��AbNA��A �A�PA+AȴA�hA+AA
�`A
bNA	G�A��A�HAI�AjAx�A�jAbNA�A|�A�\AVA�A$�A-A5?AA�A9XA%A ��@��@���@�(�@���@�ff@��T@�?}@�I�@�@���@�v�@�j@��@ꗍ@���@�\)@�v�@���@�G�@�j@�|�@��@��T@އ+@�C�@�1@�dZ@��T@�I�@�@�{@љ�@���@�I�@�+@�5?@̓u@�n�@��@�t�@�1@�@�Ĝ@�|�@�bN@+@�$�@��@��@�v�@���@�"�@�x�@��@�I�@�O�@��
@�o@��@�X@�r�@��;@�+@�-@��^@���@�p�@�G�@�7L@�/@�/@�&�@���@���@�;d@��\@�J@���@��7@�O�@��@�-@�;d@���@��!@�~�@�n�@�M�@�=q@�E�@��R@�ff@��#@���@�@���@�E�@�ff@��+@��!@�v�@�J@��^@���@���@��#@�;d@��
@�  @���@��@��D@��h@�J@�V@�^5@�^5@�V@�V@�V@�5?@�@���@�@���@�o@�C�@�l�@�l�@�dZ@�S�@�;d@�"�@�ȴ@�-@�/@�~�@�A�@��R@�=q@��T@�Ĝ@���@�|�@��P@�~�@�M�@��@��-@���@���@�hs@�7L@��@��`@���@�bN@�Z@�1@��m@�ƨ@��F@�dZ@��+@�M�@�5?@�5?@�M�@�5?@�5?@�{@���@���@�x�@�p�@�p�@�p�@�hs@�7L@�V@��@��`@��/@���@��9@�z�@�A�@��@���@��@���@�S�@��@��@��R@���@���@��\@�{@��T@��^@���@�O�@��@���@�j@�9X@�(�@��@���@�C�@�33@�+@�+@��@�o@�
=@��@���@��!@�n�@��@��@�@��-@���@��h@��@�p�@��@��@��@���@��`@�r�@��
@�ƨ@��@���@��@�dZ@�33@�
=@�ȴ@�^5@�^5@�M�@��@��^@�`B@���@�1'@�ƨ@���@�S�@�;d@��@�ȴ@���@�5?@�@���@���@��@���@���@�&�@���@��9@�I�@���@��w@���@�;d@��R@���@|:�@nOv111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�dZA�dZA�dZA�dZA�dZA�bNA�bNA�VA�7LA��A���A��#A�  A�"�A�7LA�l�AϋDAω7AσA�jA�XA�?}A��A�ZA��A���A��#A�AʾwA��A�$�A�/A�"�Aé�AÕ�A�33A�l�A�(�A�ȴA�x�A�bA���A��A�7LA�1'A���A��jA�$�A���A��yA�;dA�XA��A�VA�ffA��
A�&�A���A��A��HA�{A�VA��!A��7A��+A��PA�"�A�C�A��`A�ZA�A��A���A���A�
=A�
=A��`A�hsA�G�A�ZA���A�^5A�r�A��A�S�A��RA��A�1'A�~�A�A�A�A�E�A�ȴA�M�A��A~z�A|�jA{\)AzQ�AxjAv��Au�At�jAs�PAr{Aq�Ao�AmS�Ak�#Ah�Agt�AfM�AdQ�AcC�A^�jA[G�AY��AW�TAU�mATr�AS�#AS�AQ�AQ%AP9XANZAK\)AJAI|�AI+AH��AH(�AE�wAB�DA>�yA=�A:��A7A4$�A4  A3�A3A3�7A2��A1�-A1VA0�\A.�A-;dA,bNA+�mA*$�A(ffA&ZA$�9A${A#
=A"��A!t�A �\A�;A�7A�+A{Ap�AoA�A�A�
A|�A+A�A&�AbAA��A�A�A33A�A�AVAG�AȴAv�A�A�FA�mA��AbNA��A �A�PA+AȴA�hA+AA
�`A
bNA	G�A��A�HAI�AjAx�A�jAbNA�A|�A�\AVA�A$�A-A5?AA�A9XA%A ��@��@���@�(�@���@�ff@��T@�?}@�I�@�@���@�v�@�j@��@ꗍ@���@�\)@�v�@���@�G�@�j@�|�@��@��T@އ+@�C�@�1@�dZ@��T@�I�@�@�{@љ�@���@�I�@�+@�5?@̓u@�n�@��@�t�@�1@�@�Ĝ@�|�@�bN@+@�$�@��@��@�v�@���@�"�@�x�@��@�I�@�O�@��
@�o@��@�X@�r�@��;@�+@�-@��^@���@�p�@�G�@�7L@�/@�/@�&�@���@���@�;d@��\@�J@���@��7@�O�@��@�-@�;d@���@��!@�~�@�n�@�M�@�=q@�E�@��R@�ff@��#@���@�@���@�E�@�ff@��+@��!@�v�@�J@��^@���@���@��#@�;d@��
@�  @���@��@��D@��h@�J@�V@�^5@�^5@�V@�V@�V@�5?@�@���@�@���@�o@�C�@�l�@�l�@�dZ@�S�@�;d@�"�@�ȴ@�-@�/@�~�@�A�@��R@�=q@��T@�Ĝ@���@�|�@��P@�~�@�M�@��@��-@���@���@�hs@�7L@��@��`@���@�bN@�Z@�1@��m@�ƨ@��F@�dZ@��+@�M�@�5?@�5?@�M�@�5?@�5?@�{@���@���@�x�@�p�@�p�@�p�@�hs@�7L@�V@��@��`@��/@���@��9@�z�@�A�@��@���@��@���@�S�@��@��@��R@���@���@��\@�{@��T@��^@���@�O�@��@���@�j@�9X@�(�@��@���@�C�@�33@�+@�+@��@�o@�
=@��@���@��!@�n�@��@��@�@��-@���@��h@��@�p�@��@��@��@���@��`@�r�@��
@�ƨ@��@���@��@�dZ@�33@�
=@�ȴ@�^5@�^5@�M�@��@��^@�`B@���@�1'@�ƨ@���@�S�@�;d@��@�ȴ@���@�5?@�@���@���@��@���@���@�&�@���@��9@�I�@���@��w@���@�;d@��R@���@|:�@nOv111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�}B
�wB
��B
B
��B
ÖB
ŢB
ɺB
��B
�
B
�BB
�B
��BBDB&�B@�B�VB��B��B��B��B��B��B�B�B�BĜB
=B(�B49BC�BT�B]/B^5B^5BcTBn�Bq�Bs�Bu�Bv�By�B�B�%B�B�%B�1B�1B�%B�B�B�B�B{�Bv�Bn�BgmBbNB`BB^5BT�B6FB"�B\BB��B��B�yB�HBǮB�FB��B�VBp�BaHBR�B?}B0!B �B
=B
�5B
B
��B
�oB
�B
hsB
A�B
8RB
49B
1'B
)�B
$�B
�B
�B
hB
%B	��B	��B	�ZB	��B	��B	ÖB	�qB	�?B	�!B	��B	��B	��B	�{B	�PB	�1B	{�B	r�B	cTB	Q�B	H�B	A�B	8RB	2-B	.B	)�B	#�B	�B	�B	oB		7B	B	  B��B��B��B�B�NB�B��B��BÖB�wB�wB�qB�qB�jB�^B�RB�LB�?B�-B�!B�B�B��B��B��B��B�uB�DB�+B�B}�B|�B{�Bz�By�By�Bx�Bx�Bw�Bw�Bv�Bt�Bt�Bs�Br�Bp�Bo�Bn�Bm�Bl�Bk�BiyBgmBffBe`Be`BffBhsBq�B�B�B�B�PB�bB�bB�\B�PB�JB�DB�=B�1B�B}�B{�Bv�Bm�BhsBe`BcTBcTBaHBcTBffBhsBiyBiyBjBjBp�Bw�B|�B{�Bz�Bv�Bt�Bt�Bs�Bo�BdZB]/B^5B^5B\)BZBYBYBYBZB[#BZBZB\)B]/B^5BffBgmBiyBhsBjBw�B|�B|�B{�B|�B�B�B�Bz�Br�Bp�Bs�B~�B}�B|�By�B�B� B�+B�DB��B��B��B��B�{B�\B�B�B�B�B�+B�DB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�-B�3B�?B�^BB��B��B��B��B��B�
B�B�/B�`B�B�B��B	B	B	B	B	B	%B		7B	JB	bB	uB	�B	�B	/B	6FB	8RB	:^B	:^B	H�B	T�B	YB	\)B	\)B	]/B	]/B	^5B	^5B	_;B	_;B	_;B	bNB	gmB	s�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�B	�B	�B	�1B	�=B	�PB	�hB	�hB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�FB	�LB	�XB	�^B	�dB	�dB	�dB	�dB	�}B	��B	ÖB	ÖB	ĜB	ĜB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�/B	�/B	�/B	�5B	�5B	�HB	�NB	�TB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�}B
�wB
��B
B
��B
ÖB
ŢB
ɺB
��B
�
B
�BB
�B
��BBDB&�B@�B�VB��B��B��B��B��B��B�B�B�BĜB
=B(�B49BC�BT�B]/B^5B^5BcTBn�Bq�Bs�Bu�Bv�By�B�B�%B�B�%B�1B�1B�%B�B�B�B�B{�Bv�Bn�BgmBbNB`BB^5BT�B6FB"�B\BB��B��B�yB�HBǮB�FB��B�VBp�BaHBR�B?}B0!B �B
=B
�5B
B
��B
�oB
�B
hsB
A�B
8RB
49B
1'B
)�B
$�B
�B
�B
hB
%B	��B	��B	�ZB	��B	��B	ÖB	�qB	�?B	�!B	��B	��B	��B	�{B	�PB	�1B	{�B	r�B	cTB	Q�B	H�B	A�B	8RB	2-B	.B	)�B	#�B	�B	�B	oB		7B	B	  B��B��B��B�B�NB�B��B��BÖB�wB�wB�qB�qB�jB�^B�RB�LB�?B�-B�!B�B�B��B��B��B��B�uB�DB�+B�B}�B|�B{�Bz�By�By�Bx�Bx�Bw�Bw�Bv�Bt�Bt�Bs�Br�Bp�Bo�Bn�Bm�Bl�Bk�BiyBgmBffBe`Be`BffBhsBq�B�B�B�B�PB�bB�bB�\B�PB�JB�DB�=B�1B�B}�B{�Bv�Bm�BhsBe`BcTBcTBaHBcTBffBhsBiyBiyBjBjBp�Bw�B|�B{�Bz�Bv�Bt�Bt�Bs�Bo�BdZB]/B^5B^5B\)BZBYBYBYBZB[#BZBZB\)B]/B^5BffBgmBiyBhsBjBw�B|�B|�B{�B|�B�B�B�Bz�Br�Bp�Bs�B~�B}�B|�By�B�B� B�+B�DB��B��B��B��B�{B�\B�B�B�B�B�+B�DB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�-B�-B�3B�?B�^BB��B��B��B��B��B�
B�B�/B�`B�B�B��B	B	B	B	B	B	%B		7B	JB	bB	uB	�B	�B	/B	6FB	8RB	:^B	:^B	H�B	T�B	YB	\)B	\)B	]/B	]/B	^5B	^5B	_;B	_;B	_;B	bNB	gmB	s�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�B	�B	�B	�1B	�=B	�PB	�hB	�hB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�?B	�FB	�LB	�XB	�^B	�dB	�dB	�dB	�dB	�}B	��B	ÖB	ÖB	ĜB	ĜB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�/B	�/B	�/B	�5B	�5B	�HB	�NB	�TB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
&�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140840                              AO  ARCAADJP                                                                    20181024140840    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140840  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140840  QCF$                G�O�G�O�G�O�0               