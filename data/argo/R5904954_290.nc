CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:55Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191755  20181005191755  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              "A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��䆣&�1   @����~f@5Qhr� ��d;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     "A   A   A   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C�C  C  C  C  C   C"�C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCp  Cr  Ct  Cv  Cx�Cz�C|  C~�C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��C��C��3C�  C�  C�  C�  C��3C��3C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C��C��3C�  C�  C�  C��C�  C�  C��C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��3C�  C�  C��3C��3C��3C��3C��3C��3C��C��C�  C�  C�  C��C��C��3C�  C��C�  C��3C��3C��3C��3C�  C�  C��3C��3C�  C�  C�  C��3C��3C�  C�  C�  C��C��C�  C��3C�  C�  C��C��3C��3C�  C�  C��3C�  C��C��C��C��D   D y�D  D� D  D� D��D� D��D� DfD�fDfD�fDfD�fDfDy�D��D	�fD
  D
� D  D� D  Dy�D��D� D��Dy�D  D� D  D� D��D� DfD� D  D� D��D� D  D�fDfD� D��Dy�D  D�fD��D�fDfD� D  D�fDfD� D��D� D  Dy�D��D�fD   D �fD!  D!y�D"  D"y�D#  D#y�D$  D$� D%fD%�fD&fD&y�D'  D'� D(  D(�fD(��D)� D*  D*y�D*��D+�fD,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1fD1� D2  D2y�D3  D3�fD4  D4�fD5  D5�fD6fD6�fD7fD7� D8  D8� D8��D9y�D:fD:y�D;  D;y�D;��D<y�D=fD=�fD=��D>� D?fD?�fD@fD@y�D@��DAy�DB  DB� DB��DC� DD  DDy�DE  DE�fDFfDFy�DG  DG�fDG��DH� DH��DI� DI��DJ� DK  DK� DK��DL�fDMfDMy�DN  DN��DO�DO� DO��DP� DQ  DQy�DRfDRy�DS  DS�fDS��DTy�DU  DU�fDU�3DVy�DWfDW��DXfDX�fDYfDY�fDZ  DZ�fD[  D[� D\  D\� D]fD]� D]��D^� D_fD_� D_��D`� DafDa� Db  Db�fDcfDc�fDd  Dd� DefDe�fDf  Df� DgfDg�fDh  Dhy�Dh��Diy�Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm�fDnfDn� Do  Doy�Do��Dp� DqfDq�fDr�Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dyj�D�0�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @N{@�
>@�=qA�A%�AE�Ae�A��\A��\A��\A��\A\Aҏ\A�\A�\BG�B	G�BG�BG�B!G�B)G�B1G�B9G�BAG�BIG�BQG�BYG�BaG�BiG�BqG�ByG�B�p�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B��
Bȣ�Ḅ�BУ�Bԣ�B��
Bܣ�B�p�B��B��B��B��B���B���B���C Q�CQ�CQ�CQ�C8RC
Q�CQ�CQ�CQ�CQ�CQ�Ck�CQ�CQ�CQ�CQ�C Q�C"k�C$k�C&Q�C(Q�C*Q�C,Q�C.Q�C0Q�C2Q�C4Q�C6Q�C8Q�C:Q�C<Q�C>Q�C@Q�CBQ�CDQ�CFQ�CHQ�CJQ�CLQ�CNQ�CPQ�CRQ�CTQ�CVQ�CXQ�CZQ�C\Q�C^Q�C`Q�CbQ�CdQ�CfQ�ChQ�CjQ�ClQ�Cn8RCpQ�CrQ�CtQ�CvQ�Cxk�Czk�C|Q�C~k�C�(�C�(�C�(�C�(�C�)C�(�C�5�C�5�C�5�C�5�C�(�C�5�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�(�C�)C�)C�)C�(�C�(�C�5�C�5�C�)C�(�C�(�C�(�C�(�C�)C�)C�)C�(�C�5�C�(�C�(�C�(�C�)C�(�C�(�C�(�C�5�C�)C�(�C�(�C�(�C�5�C�(�C�(�C�B�C�5�C�5�C�5�C�(�C�)C�(�C�(�C�(�C�(�C�(�C�(�C�5�C�)C�(�C�(�C�)C�)C�)C�)C�)C�)C�5�C�5�C�(�C�(�C�(�C�5�C�5�C�)C�(�C�5�C�(�C�)C�)C�)C�)C�(�C�(�C�)C�)C�(�C�(�C�(�C�)C�)C�(�C�(�C�(�C�5�C�B�C�(�C�)C�(�C�(�C�5�C�)C�)C�(�C�(�C�)C�(�C�5�C�5�C�5�C�5�D {D �D{D�{D{D�{DD�{DD�{D�D��D�D��D�D��D�D�D	D	��D
{D
�{D{D�{D{D�DD�{DD�D{D�{D{D�{DD�{D�D�{D{D�{DD�{D{D��D�D�{DD�D{D��DD��D�D�{D{D��D�D�{DD�{D{D�DD��D {D ��D!{D!�D"{D"�D#{D#�D${D$�{D%�D%��D&�D&�D'{D'�{D({D(��D)D)�{D*{D*�D+D+��D,{D,�{D-{D-�{D.{D.�{D/{D/��D0{D0�{D1�D1�{D2{D2�D3{D3��D4{D4��D5{D5��D6�D6��D7�D7�{D8{D8�{D9D9�D:�D:�D;{D;�D<D<�D=�D=��D>D>�{D?�D?��D@�D@�DADA�DB{DB�{DCDC�{DD{DD�DE{DE��DF�DF�DG{DG��DHDH�{DIDI�{DJDJ�{DK{DK�{DLDL��DM�DM�DN{DN�HDO!HDO�{DPDP�{DQ{DQ�DR�DR�DS{DS��DTDT�DU{DU��DV�DV�DW�DW�HDX�DX��DY�DY��DZ{DZ��D[{D[�{D\{D\�{D]�D]�{D^D^�{D_�D_�{D`D`�{Da�Da�{Db{Db��Dc�Dc��Dd{Dd�{De�De��Df{Df�{Dg�Dg��Dh{Dh�DiDi�Dj{Dj�{Dk{Dk�{DlDl�{Dm{Dm��Dn�Dn�{Do{Do�DpDp�{Dq�Dq��Dr!HDr��Ds{Ds�{Dt{Dt�{Du{Du�{Dv{Dv�{Dw�Dw�{Dy\D�:�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�AuA�|�A�hsA�XA�;dA�1'A���A��TA��+A���A�%A�E�A���A�dZA��A�ffA��A���A��
A��#A��A�K�A�33A�"�A�oA�A��A�A��jA��^A��RA���A�dZA�A�A�9XA�?}A�C�A�{A��`A���A��!A���A���A�|�A�ffA�G�A�33A�(�A�$�A�VA���A�I�A��A�M�A���A��A�7LA��PA�\)A��#A�=qA�l�A���A��A��uA���A���A�oA�;dA��A��!A��HA���A��^A�  A�
=A��`A�7LA�/A���A�"�A�|�A�G�A���A���A�+A�JA�r�A�{A�z�A���A��A���A���A�ZA�%A���A���A�;dA�A���A�K�A���A��A���A���A��-A�{A���A��A�$�A�1'A�-A~��A}`BA|E�A{+A{
=Ay�#Ax(�Aul�At^5AsK�Aq��Ao�AoK�An  AlffAi�Ai�Ai;dAf�HAbȴAb1AaK�A^�A\~�A[��AZ�9AY�hAU\)AQhsAP5?AN�AL��AKS�AIVAH��AF�AES�ADAB�!AAA@��A@z�A?�A=ƨA<��A:�/A9�mA9K�A7��A6I�A5dZA4ffA3�^A2�\A0��A.��A,��A+XA*�A*jA)��A(��A(�A(1A&��A&1'A&5?A&$�A&  A%
=A#�hA"�A"z�A ��An�A
=Ar�Ar�Ar�AffAVA �A��A\)A7LA��A�DA  A��A�A��A?}A^5A�wAt�AC�Av�A1Ax�AQ�A&�A�AffAx�A�A�A
�\A�`A�hA��A33A��A�\A�A��AG�A-AoA Q�@��!@�&�@�S�@�O�@��y@���@��@�ƨ@�%@�`B@�l�@㝲@�@�%@�z�@�%@�G�@�p�@�x�@�/@��D@� �@��m@߶F@�E�@��@�J@�j@���@�
=@с@�z�@ύP@���@�^5@�o@�I�@���@��
@Ϯ@�|�@�;d@���@�O�@̃@�j@��m@�ff@ɉ7@ȃ@�j@�9X@ǝ�@�dZ@�o@��y@��@���@�Z@��@�\)@�x�@�p�@�z�@��
@��@�;d@��+@�%@� �@���@�5?@���@��D@��F@�
=@�ƨ@��@��@�l�@��w@���@� �@���@�l�@�\)@�@���@�^5@�@��h@��@��@��D@�|�@���@���@��9@���@�j@��;@��\@��T@�p�@�X@��@�b@�  @��P@�@��H@�5?@��^@��h@��h@��@�=q@�{@��^@��@�Z@��w@���@�5?@���@�&�@��@��9@� �@���@�ƨ@���@���@��@�33@��!@�M�@��@�$�@�V@��H@��!@�ff@�=q@��@�J@��T@���@�`B@��@�1@��;@��F@���@��@���@�ff@�M�@�=q@�@�`B@�&�@��/@��D@�A�@�1'@�  @��w@���@�\)@�"�@���@���@��\@�n�@�^5@�V@�-@�@��@�x�@�X@�?}@�&�@�V@��`@��u@�Z@�9X@��m@��@��P@�S�@�+@��H@�~�@�M�@�$�@�J@��@��-@�hs@��@��`@��u@�Q�@� �@��w@�dZ@�C�@�@���@��!@���@��\@�-@��^@��7@�p�@�O�@�/@���@�Ĝ@��@�bN@�9X@�9X@�9X@��@���@���@�S�@�C�@�C�@�S�@�;d@�@��H@��!@��+@�^5@�$�@�@���@�O�@�%@�z�@���@��9@�A�@��P@�S�@�;d@�
=@��@�ȴ@���@�~�@�n�@�ff@�V@��}@x��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�AuA�|�A�hsA�XA�;dA�1'A���A��TA��+A���A�%A�E�A���A�dZA��A�ffA��A���A��
A��#A��A�K�A�33A�"�A�oA�A��A�A��jA��^A��RA���A�dZA�A�A�9XA�?}A�C�A�{A��`A���A��!A���A���A�|�A�ffA�G�A�33A�(�A�$�A�VA���A�I�A��A�M�A���A��A�7LA��PA�\)A��#A�=qA�l�A���A��A��uA���A���A�oA�;dA��A��!A��HA���A��^A�  A�
=A��`A�7LA�/A���A�"�A�|�A�G�A���A���A�+A�JA�r�A�{A�z�A���A��A���A���A�ZA�%A���A���A�;dA�A���A�K�A���A��A���A���A��-A�{A���A��A�$�A�1'A�-A~��A}`BA|E�A{+A{
=Ay�#Ax(�Aul�At^5AsK�Aq��Ao�AoK�An  AlffAi�Ai�Ai;dAf�HAbȴAb1AaK�A^�A\~�A[��AZ�9AY�hAU\)AQhsAP5?AN�AL��AKS�AIVAH��AF�AES�ADAB�!AAA@��A@z�A?�A=ƨA<��A:�/A9�mA9K�A7��A6I�A5dZA4ffA3�^A2�\A0��A.��A,��A+XA*�A*jA)��A(��A(�A(1A&��A&1'A&5?A&$�A&  A%
=A#�hA"�A"z�A ��An�A
=Ar�Ar�Ar�AffAVA �A��A\)A7LA��A�DA  A��A�A��A?}A^5A�wAt�AC�Av�A1Ax�AQ�A&�A�AffAx�A�A�A
�\A�`A�hA��A33A��A�\A�A��AG�A-AoA Q�@��!@�&�@�S�@�O�@��y@���@��@�ƨ@�%@�`B@�l�@㝲@�@�%@�z�@�%@�G�@�p�@�x�@�/@��D@� �@��m@߶F@�E�@��@�J@�j@���@�
=@с@�z�@ύP@���@�^5@�o@�I�@���@��
@Ϯ@�|�@�;d@���@�O�@̃@�j@��m@�ff@ɉ7@ȃ@�j@�9X@ǝ�@�dZ@�o@��y@��@���@�Z@��@�\)@�x�@�p�@�z�@��
@��@�;d@��+@�%@� �@���@�5?@���@��D@��F@�
=@�ƨ@��@��@�l�@��w@���@� �@���@�l�@�\)@�@���@�^5@�@��h@��@��@��D@�|�@���@���@��9@���@�j@��;@��\@��T@�p�@�X@��@�b@�  @��P@�@��H@�5?@��^@��h@��h@��@�=q@�{@��^@��@�Z@��w@���@�5?@���@�&�@��@��9@� �@���@�ƨ@���@���@��@�33@��!@�M�@��@�$�@�V@��H@��!@�ff@�=q@��@�J@��T@���@�`B@��@�1@��;@��F@���@��@���@�ff@�M�@�=q@�@�`B@�&�@��/@��D@�A�@�1'@�  @��w@���@�\)@�"�@���@���@��\@�n�@�^5@�V@�-@�@��@�x�@�X@�?}@�&�@�V@��`@��u@�Z@�9X@��m@��@��P@�S�@�+@��H@�~�@�M�@�$�@�J@��@��-@�hs@��@��`@��u@�Q�@� �@��w@�dZ@�C�@�@���@��!@���@��\@�-@��^@��7@�p�@�O�@�/@���@�Ĝ@��@�bN@�9X@�9X@�9X@��@���@���@�S�@�C�@�C�@�S�@�;d@�@��H@��!@��+@�^5@�$�@�@���@�O�@�%@�z�@���@��9@�A�@��P@�S�@�;d@�
=@��@�ȴ@���@�~�@�n�@�ff@�V@��}@x��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B$�B+B.B,B)�B+B-B/B5?B7LB;dBA�BL�BL�BN�BR�BXBZB[#B\)BaHBiyBm�Bq�Bq�Bp�Bp�Bt�Bz�B� B�+B�=B�=B�=B�DB�DB�PB�hB�oB�oB�oB�{B��B��B��B��B�B��B��B�B�B�wB��B��BhsBN�B9XB2-B-B#�B�BJB��B��BuB�BuB1B%B��B�B�B�mB�HB�/B�B��B��BB�jB�RB�!B��B��B��B��B��B�{B�hB�VB�7B�%B�B{�Bv�BffB:^B	7B
�NB
�B
��B
� B
v�B
r�B
r�B
r�B
l�B
e`B
^5B
\)B
S�B
I�B
9XB
33B
/B
&�B
�B
�B
VB	��B	�B	�B	�B	�BB	ȴB	ŢB	�}B	�3B	��B	��B	�JB	{�B	\)B	?}B	49B	'�B	 �B	�B	+B	B��B�B�yB�TB�;B�)B�B��B��BƨB��B�jB�XB�-B�B��B��B��B��B��B�oB�VB�DB�JB�\B�\B�VB�JB�DB�=B�7B�=B�=B�7B�%B�B�B�B�B�B�B�B�B�B�%B�%B�%B�B�B�B�B� B}�Bz�Bx�Bv�Bs�Bq�Bp�Bo�Bn�Bm�Bk�BjBgmBcTB]/BYBXBXBW
BVBW
BS�BQ�BP�BQ�BW
BYBaHBgmBo�Bp�Bq�Bu�Bv�By�B{�B}�B}�B|�B{�Bw�Bq�BhsBe`BdZBgmBk�Br�Bt�Bv�Bw�B{�B�B�B�B�B�B�B�B|�Bz�Bu�Bp�Bn�Bp�Bt�Bw�B�B�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�?B�LB�LB�FB�XB��BǮBƨBȴB��B��B��B�#B�/B�B�B�BB�TB�sB�B��B��B��B��B��B	B	
=B	VB	hB	hB	uB	�B	�B	�B	#�B	$�B	'�B	.B	1'B	7LB	8RB	7LB	9XB	<jB	;dB	<jB	B�B	E�B	F�B	G�B	K�B	P�B	R�B	S�B	VB	\)B	aHB	dZB	k�B	n�B	n�B	o�B	s�B	u�B	v�B	v�B	x�B	{�B	}�B	}�B	~�B	� B	~�B	�B	�B	�%B	�DB	�JB	�PB	�PB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�?B	�FB	�FB	�LB	�LB	�RB	�^B	�jB	�wB	�wB	�}B	��B	��B	ÖB	ĜB	ŢB	ŢB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�5B	�;B	�BB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
+B
%B
B
B
B
B
%B
%B
+B
1B
	7B
DB
PB
�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�B�B�B�B�B�B�B�B�B�B$�B+B.B,B)�B+B-B/B5?B7LB;dBA�BL�BL�BN�BR�BXBZB[#B\)BaHBiyBm�Bq�Bq�Bp�Bp�Bt�Bz�B� B�+B�=B�=B�=B�DB�DB�PB�hB�oB�oB�oB�{B��B��B��B��B�B��B��B�B�B�wB��B��BhsBN�B9XB2-B-B#�B�BJB��B��BuB�BuB1B%B��B�B�B�mB�HB�/B�B��B��BB�jB�RB�!B��B��B��B��B��B�{B�hB�VB�7B�%B�B{�Bv�BffB:^B	7B
�NB
�B
��B
� B
v�B
r�B
r�B
r�B
l�B
e`B
^5B
\)B
S�B
I�B
9XB
33B
/B
&�B
�B
�B
VB	��B	�B	�B	�B	�BB	ȴB	ŢB	�}B	�3B	��B	��B	�JB	{�B	\)B	?}B	49B	'�B	 �B	�B	+B	B��B�B�yB�TB�;B�)B�B��B��BƨB��B�jB�XB�-B�B��B��B��B��B��B�oB�VB�DB�JB�\B�\B�VB�JB�DB�=B�7B�=B�=B�7B�%B�B�B�B�B�B�B�B�B�B�%B�%B�%B�B�B�B�B� B}�Bz�Bx�Bv�Bs�Bq�Bp�Bo�Bn�Bm�Bk�BjBgmBcTB]/BYBXBXBW
BVBW
BS�BQ�BP�BQ�BW
BYBaHBgmBo�Bp�Bq�Bu�Bv�By�B{�B}�B}�B|�B{�Bw�Bq�BhsBe`BdZBgmBk�Br�Bt�Bv�Bw�B{�B�B�B�B�B�B�B�B|�Bz�Bu�Bp�Bn�Bp�Bt�Bw�B�B�PB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�!B�?B�LB�LB�FB�XB��BǮBƨBȴB��B��B��B�#B�/B�B�B�BB�TB�sB�B��B��B��B��B��B	B	
=B	VB	hB	hB	uB	�B	�B	�B	#�B	$�B	'�B	.B	1'B	7LB	8RB	7LB	9XB	<jB	;dB	<jB	B�B	E�B	F�B	G�B	K�B	P�B	R�B	S�B	VB	\)B	aHB	dZB	k�B	n�B	n�B	o�B	s�B	u�B	v�B	v�B	x�B	{�B	}�B	}�B	~�B	� B	~�B	�B	�B	�%B	�DB	�JB	�PB	�PB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�3B	�9B	�?B	�FB	�FB	�LB	�LB	�RB	�^B	�jB	�wB	�wB	�}B	��B	��B	ÖB	ĜB	ŢB	ŢB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�5B	�5B	�;B	�BB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
+B
%B
B
B
B
B
%B
%B
+B
1B
	7B
DB
PB
�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.32 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191755                              AO  ARCAADJP                                                                    20181005191755    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191755  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191755  QCF$                G�O�G�O�G�O�8000            