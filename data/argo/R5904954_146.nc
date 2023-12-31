CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:22Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191722  20181005191722  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��䂧�1   @������@5!$�/�dzvȴ9X1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BXffB`ffBg��Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0�C2�C4�C6  C8  C:  C<�C>�C@  CB  CD  CF  CH  CJ  CK�fCM�fCP  CR�CT�CV  CX  CZ  C\  C^�C`�Cb�Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCw�fCz  C|�C~  C��C��C�  C��C�  C��C��3C��fC��3C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C��3C��3C�  C��3C�  C�  C��3C�  C��C��3C��C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C��C�  C�  C��C��C�  C�  C��C��C��C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C��3C��3C��3C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��3C��3C��fC�  C��C�  C�  C�  C��C�  C�  D fD � DfD�fD  D� D  D�fDfD� D��Dy�D  D� D  D� D��Dy�D��D	� D
  D
� DfD�fDfD� DfD� D��Dy�D��Dy�D  D� D��D� D  D�fDfD�fDfD��D  D� D  D� D  D�fD  D� D  Dy�D��Dy�D  D�fD  D� DfD� D  D�fD  D� D   D � D!fD!� D"  D"y�D#  D#�fD$  D$� D%  D%�fD&fD&�fD'  D'y�D(  D(�fD)  D)� D*fD*� D*��D+� D,fD,� D,��D-y�D-��D.�fD/  D/y�D0  D0� D1  D1� D1��D2�fD3fD3� D3��D4y�D5  D5�fD6fD6y�D6��D7� D8  D8� D8��D9� D:fD:�fD;fD;�fD;��D<� D=  D=� D>fD>�fD?fD?� D?��D@� DAfDA� DA��DBy�DC  DC�fDDfDD� DE  DE� DE��DF� DG  DG� DG��DHy�DH��DIy�DI��DJy�DK  DK� DK��DL� DMfDM�fDNfDN� DO  DO�fDPfDP� DQfDQ� DR  DR�fDS�DS�fDT  DT�fDU  DU� DVfDV�fDW  DW� DXfDX� DY  DY� DZ  DZ�fD[�D[�fD\  D\�fD]  D]y�D]��D^y�D_  D_��D`fD`� Da  Day�DbfDb� Db��Dc�fDd  Dds3De  De��Df  Dfy�Df��Dgy�Dh  Dh�fDifDi�fDjfDj� Dk  Dk�fDk��Dly�Dm  Dm�fDn  Dny�Do  Do� Do��Dp� Dq  Dqs3Dr  Dry�Ds  Dsy�Ds��Dt� Du  Duy�Dv  Dv� Dw  Dw� Dy��D�6fD�L)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�p�@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BA�BIG�BQG�BY�Ba�Bh�HBp�HByG�B���B���B���B���B���B���B���B���B���B���B���B���B���B�p�B���B���B���Bģ�Bȣ�Ḅ�BУ�Bԣ�Bأ�Bܣ�B��B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�CQ�C
Q�CQ�Ck�Ck�CQ�Ck�CQ�CQ�CQ�CQ�CQ�C Q�C"Q�C$Q�C&Q�C(Q�C*k�C,k�C.Q�C0k�C2k�C4k�C6Q�C8Q�C:Q�C<k�C>k�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CL8RCN8RCPQ�CRk�CTk�CVQ�CXQ�CZQ�C\Q�C^k�C`k�Cbk�CdQ�Cf8RChQ�CjQ�ClQ�CnQ�CpQ�CrQ�CtQ�Cv8RCx8RCzQ�C|k�C~Q�C�5�C�5�C�(�C�5�C�(�C�5�C�)C�\C�)C�)C�)C�(�C�(�C�(�C�(�C�(�C�(�C�)C�(�C�)C�)C�(�C�)C�(�C�(�C�)C�(�C�5�C�)C�5�C�)C�)C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�B�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�(�C�)C�)C�(�C�(�C�(�C�(�C�(�C�5�C�(�C�5�C�(�C�(�C�5�C�5�C�(�C�(�C�5�C�5�C�5�C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�)C�)C�(�C�(�C�)C�(�C�(�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�)C�)C�)C�)C�(�C�5�C�(�C�(�C�)C�)C�)C�(�C�(�C�(�C�(�C�5�C�5�C�(�C�)C�(�C�(�C�)C�)C�\C�(�C�5�C�(�C�(�C�(�C�5�C�(�C�(�D �D �{D�D��D{D�{D{D��D�D�{DD�D{D�{D{D�{DD�D	D	�{D
{D
�{D�D��D�D�{D�D�{DD�DD�D{D�{DD�{D{D��D�D��D�D�HD{D�{D{D�{D{D��D{D�{D{D�DD�D{D��D{D�{D�D�{D{D��D{D�{D {D �{D!�D!�{D"{D"�D#{D#��D${D$�{D%{D%��D&�D&��D'{D'�D({D(��D){D)�{D*�D*�{D+D+�{D,�D,�{D-D-�D.D.��D/{D/�D0{D0�{D1{D1�{D2D2��D3�D3�{D4D4�D5{D5��D6�D6�D7D7�{D8{D8�{D9D9�{D:�D:��D;�D;��D<D<�{D={D=�{D>�D>��D?�D?�{D@D@�{DA�DA�{DBDB�DC{DC��DD�DD�{DE{DE�{DFDF�{DG{DG�{DHDH�DIDI�DJDJ�DK{DK�{DLDL�{DM�DM��DN�DN�{DO{DO��DP�DP�{DQ�DQ�{DR{DR��DS!HDS��DT{DT��DU{DU�{DV�DV��DW{DW�{DX�DX�{DY{DY�{DZ{DZ��D[!HD[��D\{D\��D]{D]�D^D^�D_{D_�HD`�D`�{Da{Da�Db�Db�{DcDc��Dd{Dd��De{De�HDf{Df�DgDg�Dh{Dh��Di�Di��Dj�Dj�{Dk{Dk��DlDl�Dm{Dm��Dn{Dn�Do{Do�{DpDp�{Dq{Dq��Dr{Dr�Ds{Ds�DtDt�{Du{Du�Dv{Dv�{Dw{Dw�{Dy�D�@�D�Vf11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A���A���A���A���A���A���A���A���A���A���A���A���A�A���A�  A�  A�  A�A�A�A�A�A�1A�
=A�1A�z�A�ȴA��A�Aħ�A�p�A���A���A�p�A�E�A�bNA��
A��A��yA���A���A���A�bNA���A�-A���A�t�A�ZA�Q�A��^A��A�?}A�=qA���A�$�A�ȴA��;A���A��
A��DA�|�A�G�A�&�A�G�A��A�9XA�JA�M�A�|�A��TA�  A�5?A���A��PA�t�A�VA���A�=qA���A��mA��A�33A��uA�VA��
A�&�A���A�&�A�  A�A~z�A~E�A}�-Ay`BAt=qAq�AlM�AiO�AfĜAehsAb��Aa
=A^=qA[A[/AY��AW�AUAS�AQ��AO��ANn�AM+AI
=AG�AEhsADZACdZABAA"�A@1A<-A933A81A7?}A5�^A3�#A3�A2�!A1��A0�A/O�A.�/A-��A+��A*=qA(�!A&�DA%O�A%�A$�HA$Q�A"�A �A�A��A�!A+Az�A$�A�FA��A��A�HA�HA��AbNA��AC�A��A��A^5AJAA	+A��A��An�A��A\)A�DA�7AjA|�A �9@�X@��F@�=q@�`B@��F@�K�@�^5@���@�@�~�@�@��;@��H@�\@���@�7L@��`@�u@�(�@�K�@�n�@��@���@�ff@�bN@�v�@�7@� �@ޏ\@��T@��@�|�@���@�5?@ف@�(�@�o@�V@ղ-@��/@�ȴ@�$�@��#@�@с@��/@�A�@��
@�t�@�ff@�p�@��`@�A�@���@ʇ+@�ff@�5?@�{@�J@��T@�@ȃ@��y@Ə\@�-@�O�@�%@���@���@���@ēu@�dZ@�{@��@�X@��^@�5?@��@�Z@�ff@�r�@�~�@�V@�
=@��7@��j@�;d@��R@�~�@��^@�?}@�r�@���@��w@�
=@��T@�hs@���@�(�@��@��!@���@��/@�
=@��@�z�@�z�@�Z@���@���@�r�@�j@�bN@�r�@�(�@��F@���@��@���@��9@��D@�Ĝ@��/@��7@��7@�X@��D@���@���@��@��
@�C�@���@��@��@��@��H@�ȴ@���@�ff@���@�&�@�Ĝ@�(�@�  @� �@�(�@�1'@�A�@�(�@�bN@�z�@�j@�ƨ@���@���@�\)@���@�~�@�{@�G�@��/@��9@�Ĝ@��9@��@�1'@��
@�t�@�+@��y@���@�ȴ@��@��!@��+@�ff@�E�@�{@�@���@��T@���@�O�@��@�I�@�  @��w@���@���@���@���@�S�@�"�@�|�@���@��w@��@���@�+@���@�
=@�o@��@��@��y@�ff@�J@���@�?}@��@�j@�9X@�9X@� �@��m@���@��@�|�@�dZ@�l�@��P@���@��@�I�@��F@���@�I�@�Z@��D@��9@�z�@��u@���@���@���@��9@��@���@��D@�1'@��
@���@�K�@�o@��@�v�@�^5@�{@���@�x�@�`B@�hs@�X@�/@���@���@�A�@� �@�b@�1@��w@�S�@�33@��y@��\@�ff@�J@��^@��@�&�@�%@���@��/@���@��9@�Ĝ@�r�@�ƨ@��@�l�@�C�@�o@��@���@�M�@���@��7@�G�@�&�@�%@�%@��@�X@�G�@���@��9@���@�/@��`@��@�?}@�z�@�j@�bN@�r�@�bN@�Z@��u@��j@��j@�/@�x�@�`B@�V@��j@��j@~͟@p]d@`�?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A���A���A���A���A���A���A���A���A���A���A���A���A�A���A�  A�  A�  A�A�A�A�A�A�1A�
=A�1A�z�A�ȴA��A�Aħ�A�p�A���A���A�p�A�E�A�bNA��
A��A��yA���A���A���A�bNA���A�-A���A�t�A�ZA�Q�A��^A��A�?}A�=qA���A�$�A�ȴA��;A���A��
A��DA�|�A�G�A�&�A�G�A��A�9XA�JA�M�A�|�A��TA�  A�5?A���A��PA�t�A�VA���A�=qA���A��mA��A�33A��uA�VA��
A�&�A���A�&�A�  A�A~z�A~E�A}�-Ay`BAt=qAq�AlM�AiO�AfĜAehsAb��Aa
=A^=qA[A[/AY��AW�AUAS�AQ��AO��ANn�AM+AI
=AG�AEhsADZACdZABAA"�A@1A<-A933A81A7?}A5�^A3�#A3�A2�!A1��A0�A/O�A.�/A-��A+��A*=qA(�!A&�DA%O�A%�A$�HA$Q�A"�A �A�A��A�!A+Az�A$�A�FA��A��A�HA�HA��AbNA��AC�A��A��A^5AJAA	+A��A��An�A��A\)A�DA�7AjA|�A �9@�X@��F@�=q@�`B@��F@�K�@�^5@���@�@�~�@�@��;@��H@�\@���@�7L@��`@�u@�(�@�K�@�n�@��@���@�ff@�bN@�v�@�7@� �@ޏ\@��T@��@�|�@���@�5?@ف@�(�@�o@�V@ղ-@��/@�ȴ@�$�@��#@�@с@��/@�A�@��
@�t�@�ff@�p�@��`@�A�@���@ʇ+@�ff@�5?@�{@�J@��T@�@ȃ@��y@Ə\@�-@�O�@�%@���@���@���@ēu@�dZ@�{@��@�X@��^@�5?@��@�Z@�ff@�r�@�~�@�V@�
=@��7@��j@�;d@��R@�~�@��^@�?}@�r�@���@��w@�
=@��T@�hs@���@�(�@��@��!@���@��/@�
=@��@�z�@�z�@�Z@���@���@�r�@�j@�bN@�r�@�(�@��F@���@��@���@��9@��D@�Ĝ@��/@��7@��7@�X@��D@���@���@��@��
@�C�@���@��@��@��@��H@�ȴ@���@�ff@���@�&�@�Ĝ@�(�@�  @� �@�(�@�1'@�A�@�(�@�bN@�z�@�j@�ƨ@���@���@�\)@���@�~�@�{@�G�@��/@��9@�Ĝ@��9@��@�1'@��
@�t�@�+@��y@���@�ȴ@��@��!@��+@�ff@�E�@�{@�@���@��T@���@�O�@��@�I�@�  @��w@���@���@���@���@�S�@�"�@�|�@���@��w@��@���@�+@���@�
=@�o@��@��@��y@�ff@�J@���@�?}@��@�j@�9X@�9X@� �@��m@���@��@�|�@�dZ@�l�@��P@���@��@�I�@��F@���@�I�@�Z@��D@��9@�z�@��u@���@���@���@��9@��@���@��D@�1'@��
@���@�K�@�o@��@�v�@�^5@�{@���@�x�@�`B@�hs@�X@�/@���@���@�A�@� �@�b@�1@��w@�S�@�33@��y@��\@�ff@�J@��^@��@�&�@�%@���@��/@���@��9@�Ĝ@�r�@�ƨ@��@�l�@�C�@�o@��@���@�M�@���@��7@�G�@�&�@�%@�%@��@�X@�G�@���@��9@���@�/@��`@��@�?}@�z�@�j@�bN@�r�@�bN@�Z@��u@��j@��j@�/@�x�@�`B@�V@��j@��j@~͟@p]d@`�?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B{�B{�B{�B{�B{�B{�Bz�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�Bz�Bk�BZBt�B|�Bv�Bv�B~�B�JB�{B��B��B��B��B�B�B��B�B�B��B��B��B��B�oB�1By�Bt�Bk�B^5BQ�BJ�B;dB.B�B�B+B��B�B�#BƨB�?B��B��B~�BgmBP�BD�B8RB0!B �BhBB
��B
�B
�ZB
��B
�!B
��B
�\B
�%B
v�B
n�B
ffB
_;B
O�B
B�B
8RB
5?B
.B
oB	�B	�B	�qB	��B	��B	�VB	�B	s�B	e`B	XB	R�B	L�B	<jB	49B	+B	�B	{B	PB	B��B�B�mB�NB�5B�B��B��BŢB�wB�dB�RB�FB�'B�!B�!B�!B�B�B�B��B��B��B��B��B��B��B��B�uB�uB�hB�bB�VB�PB�DB�+B�Bx�Bq�Bk�BgmB`BB]/BYBW
BVBT�BS�BQ�BO�BL�BH�BF�BD�BD�BB�B@�B>wB=qB;dB8RB6FB5?B33B33B2-B2-B1'B1'B0!B0!B/B.B.B.B.B.B.B.B-B-B-B-B,B,B,B-B.B.B/B0!B1'B1'B2-B1'B1'B1'B2-B33B33B33B33B5?B5?B5?B5?B49B49B49B49B49B49B49B49B49B6FB6FB6FB7LB7LB8RB8RB8RB<jBB�BB�BD�BG�BJ�BJ�BK�BM�BP�BR�BN�BN�BR�BW
B]/B`BB]/BW
BS�BT�BVBYB\)B^5BdZBdZBdZBffBgmBk�Bl�Bl�Bm�Bq�Bs�Bu�By�B|�B~�B� B�B��B��B��B��B��B��B�3B��BĜBȴB��B��B��B��B��B��B�B�BB�B�B��B	B	B	B	B	B	B	%B	
=B	PB	VB	VB	VB	\B	\B	bB	{B	�B	"�B	#�B	%�B	(�B	,B	.B	/B	0!B	49B	<jB	@�B	D�B	H�B	J�B	K�B	M�B	O�B	Q�B	T�B	S�B	T�B	W
B	]/B	]/B	^5B	^5B	^5B	_;B	cTB	iyB	o�B	s�B	u�B	w�B	y�B	|�B	|�B	~�B	�B	�B	�B	�B	�7B	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�B	�B	�B	�B	�-B	�3B	�?B	�RB	�XB	�wB	��B	B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�5B	�BB	�HB	�BB	�HB	�NB	�NB	�TB	�`B	�`B	�`B	�`B	�ZB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B	�]B
�B
!�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B{�B{�B{�B{�B{�B{�Bz�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�B{�Bz�Bk�BZBt�B|�Bv�Bv�B~�B�JB�{B��B��B��B��B�B�B��B�B�B��B��B��B��B�oB�1By�Bt�Bk�B^5BQ�BJ�B;dB.B�B�B+B��B�B�#BƨB�?B��B��B~�BgmBP�BD�B8RB0!B �BhBB
��B
�B
�ZB
��B
�!B
��B
�\B
�%B
v�B
n�B
ffB
_;B
O�B
B�B
8RB
5?B
.B
oB	�B	�B	�qB	��B	��B	�VB	�B	s�B	e`B	XB	R�B	L�B	<jB	49B	+B	�B	{B	PB	B��B�B�mB�NB�5B�B��B��BŢB�wB�dB�RB�FB�'B�!B�!B�!B�B�B�B��B��B��B��B��B��B��B��B�uB�uB�hB�bB�VB�PB�DB�+B�Bx�Bq�Bk�BgmB`BB]/BYBW
BVBT�BS�BQ�BO�BL�BH�BF�BD�BD�BB�B@�B>wB=qB;dB8RB6FB5?B33B33B2-B2-B1'B1'B0!B0!B/B.B.B.B.B.B.B.B-B-B-B-B,B,B,B-B.B.B/B0!B1'B1'B2-B1'B1'B1'B2-B33B33B33B33B5?B5?B5?B5?B49B49B49B49B49B49B49B49B49B6FB6FB6FB7LB7LB8RB8RB8RB<jBB�BB�BD�BG�BJ�BJ�BK�BM�BP�BR�BN�BN�BR�BW
B]/B`BB]/BW
BS�BT�BVBYB\)B^5BdZBdZBdZBffBgmBk�Bl�Bl�Bm�Bq�Bs�Bu�By�B|�B~�B� B�B��B��B��B��B��B��B�3B��BĜBȴB��B��B��B��B��B��B�B�BB�B�B��B	B	B	B	B	B	B	%B	
=B	PB	VB	VB	VB	\B	\B	bB	{B	�B	"�B	#�B	%�B	(�B	,B	.B	/B	0!B	49B	<jB	@�B	D�B	H�B	J�B	K�B	M�B	O�B	Q�B	T�B	S�B	T�B	W
B	]/B	]/B	^5B	^5B	^5B	_;B	cTB	iyB	o�B	s�B	u�B	w�B	y�B	|�B	|�B	~�B	�B	�B	�B	�B	�7B	�JB	�PB	�VB	�VB	�VB	�VB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�B	�B	�B	�B	�-B	�3B	�?B	�RB	�XB	�wB	��B	B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�;B	�5B	�BB	�HB	�BB	�HB	�NB	�NB	�TB	�`B	�`B	�`B	�`B	�ZB	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
%B	�]B
�B
!�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191722                              AO  ARCAADJP                                                                    20181005191722    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191722  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191722  QCF$                G�O�G�O�G�O�8000            