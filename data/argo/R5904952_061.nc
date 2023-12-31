CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  1   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:18Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  >4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?h   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  D,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E`   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  J$   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  N�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  T�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  V   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  _�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  `�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 4  e�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  k�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    k�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    n�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    q�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  t�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    t�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    t�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    t�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    t�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  t�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    u8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    uH   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    uL   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         u\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         u`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ud   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    uhArgo profile    3.1 1.2 19500101000000  20181005190518  20181005190518  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               =A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׾�-!�1   @׾���Ӓ@1N��+�c�n��P1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      =A   A   A   @@  @�  @�  A   A   A@  A`  A���A�  A�33A�33A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�fC�fC  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D�fDfD� D  D� DfD� D  Dy�D  D�fD  D� D	  D	� D
  D
� DfD�fDfD�fD��Dy�D  D� D  D� D  D� DfD�fD  Dy�D  D� D  Dy�D  D� D��D� D  D�fDfD�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @Q�@���@���Az�A$z�ADz�Adz�A�
>A�=qA�p�A�p�A�=qA�=qA�=qA�=qB�B	�B�B�RB!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B�\)B��\B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\Bď\Bȏ\B̏\BЏ\Bԏ\B؏\B܏\B�B�\B�\B�\)B�\)B�\B��\B��\C G�CG�CG�CG�CG�C
G�CG�CG�CG�CG�CG�CG�C.C.CG�CG�C G�C"G�C$G�C&G�C(G�C*aHC,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@aHCBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�0�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�D �D ��D�D��D�D�RDRD��D�D��DRD��D�D��D�D�RD�D��D	�D	��D
�D
��DRD�RDRD�RD�D��D�D��D�D��D�D��DRD�RD�D��D�D��D�D��D�D��D�D��D�D�RDRD�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�dZA�bNA�dZA�`BA�^5A�`BA�dZA�dZA�`BA�dZA�jA�l�A�l�A�hsA�ffA�jA�jA�E�A���A�%AՍPA�ȴA�=qAӃAӧ�A�$�A�l�A��A���A�?}AН�A�`BA�5?A��;A�n�A�&�A���A�5?A͉7A�E�A̼jA�VA�+A�z�A�/A�hsA�\)A��#Aǝ�A��`A��A�M�A�|�A�ffA��A�p�A��!A��A�XA��A���A�l�A��A��-A��A�"�A�"�A�
=A�E�A�C�A�{A��A�7LA�/A�VA�\)A��PA�
=A�dZA��#A��A�$�A�+A��A�VA�"�A��HA�7LA��FA�VA��RA�ȴA�/A��A�`BA���A�ZA�bA�-A�l�A�ZA�ffA��^A��A��FA��/A~n�A|��Av�uAq��An�HAjM�Aj��Ai&�Ae�A_��A\��AY�AU%AQ�-AO`BAM7LAKK�AE��AEC�AC�A@��A@A?+A=dZA;�TA:1A89XA6�A4�9A0��A.=qA-�TA-;dA+�;A+�A(n�A&��A$�\A#�
A#C�A"r�A"�A �HA A�A�#A�FA�7AoA�`A�+A��A�Ar�A�A��AA~�AffA�DAjA��A&�A��A�+A�AK�A9XA�^AO�A9XA"�Ar�A��A�wA��A=qA�wA�TAA-A��A1'A+AO�A��AI�A�TA	��A��A%A�AK�A�TA �uA E�A bNA �+A M�@��@�ff@�  @�~�@�{@�&�@�@�;d@�ff@�&�@�I�@�S�@���@��@�@�j@�Z@�
=@�dZ@�ƨ@��@�t�@�"�@�
=@��@�@�1@���@�@�1@�;d@��@�V@�@�%@�9@���@�t�@�=q@�z�@�b@��@ޗ�@�ƨ@��`@�j@��
@ް!@܃@ۍP@��;@�&�@ܓu@۾w@�\)@���@�7L@��/@��m@�"�@�^5@���@�`B@�7L@�1@ӕ�@��@��H@�v�@҇+@҇+@���@�x�@�O�@�G�@�?}@�G�@�?}@��@�Z@��@���@���@�\)@�"�@�ȴ@�V@�{@ə�@���@�b@��H@�33@�^5@�7L@�Ĝ@�9X@�(�@��
@�M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�bNA�dZA�bNA�dZA�`BA�^5A�`BA�dZA�dZA�`BA�dZA�jA�l�A�l�A�hsA�ffA�jA�jA�E�A���A�%AՍPA�ȴA�=qAӃAӧ�A�$�A�l�A��A���A�?}AН�A�`BA�5?A��;A�n�A�&�A���A�5?A͉7A�E�A̼jA�VA�+A�z�A�/A�hsA�\)A��#Aǝ�A��`A��A�M�A�|�A�ffA��A�p�A��!A��A�XA��A���A�l�A��A��-A��A�"�A�"�A�
=A�E�A�C�A�{A��A�7LA�/A�VA�\)A��PA�
=A�dZA��#A��A�$�A�+A��A�VA�"�A��HA�7LA��FA�VA��RA�ȴA�/A��A�`BA���A�ZA�bA�-A�l�A�ZA�ffA��^A��A��FA��/A~n�A|��Av�uAq��An�HAjM�Aj��Ai&�Ae�A_��A\��AY�AU%AQ�-AO`BAM7LAKK�AE��AEC�AC�A@��A@A?+A=dZA;�TA:1A89XA6�A4�9A0��A.=qA-�TA-;dA+�;A+�A(n�A&��A$�\A#�
A#C�A"r�A"�A �HA A�A�#A�FA�7AoA�`A�+A��A�Ar�A�A��AA~�AffA�DAjA��A&�A��A�+A�AK�A9XA�^AO�A9XA"�Ar�A��A�wA��A=qA�wA�TAA-A��A1'A+AO�A��AI�A�TA	��A��A%A�AK�A�TA �uA E�A bNA �+A M�@��@�ff@�  @�~�@�{@�&�@�@�;d@�ff@�&�@�I�@�S�@���@��@�@�j@�Z@�
=@�dZ@�ƨ@��@�t�@�"�@�
=@��@�@�1@���@�@�1@�;d@��@�V@�@�%@�9@���@�t�@�=q@�z�@�b@��@ޗ�@�ƨ@��`@�j@��
@ް!@܃@ۍP@��;@�&�@ܓu@۾w@�\)@���@�7L@��/@��m@�"�@�^5@���@�`B@�7L@�1@ӕ�@��@��H@�v�@҇+@҇+@���@�x�@�O�@�G�@�?}@�G�@�?}@��@�Z@��@���@���@�\)@�"�@�ȴ@�V@�{@ə�@���@�b@��H@�33@�^5@�7L@�Ĝ@�9X@�(�@��
@�M�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
1'B
1'B
1'B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
.B
6FB
;dB
6FB
/B
!�B
�B
DB
$�B
$�B
bB
�B
<jB
bNB
m�B
q�B
|�B
�7B
��B
��B
�LB
��B
�ZB
��B
��B	7B,B<jBB�B]/Bl�Bp�Br�B~�B�DB��B�9B�B��B  B
=B0!BQ�Be`Bu�Bt�B|�B�B�=B��B��B��B�B�'B��B�B�LB�!B��B�LB�FB�B�B�!B�7BdZBS�BG�B49B�B�sB��B�%BW
BP�BQ�BI�BC�B(�B
��B
�BJB�BuBB
�HB
�B
{�B
ffB
>wB
�B
uB	��B	��B	��B	z�B	�uB	��B	�B	gmB	XB	I�B	-B	uB	{B	PB��B�BB�BB�B��B��BǮB��B�dB�LB�?B�-B�B�B�B�qB�dB�^B�qB�jB�?B�B�B��B��B��B�B�-B�'B�'B�!B�B�-B�?B�9B�B��B�B�-B�LB�qB��BÖB��B��B��B��B��B�B�B�/B�)B�#B�mB�B�B�yB�sB�yB��B	bB	{B	oB	VB	DB	PB	oB	�B	�B	{B	bB	1B��B��B��B�B�5B�B�B�5B�BB�5B�
B��B��B��B�B�B�B�
B�B�
B�B��B��B��B�
B�B�)B�;B�sB��B��B	B	B	DB		7B		7B	
=B	PB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	(�B	'�B	)�B	+B	&�B	0!B	?}B	L�B	M�B	O�B	P�B	J�B	F�B	R�B	^5B	^5B	\)B	[#B	[#B	dZB	gmB	gmB	ffB	dZB	cTB	dZB	gmB	gmB	hsB	n�B	p�B	s�B	t�B	v�B	x�B	z�B	{�B	{�B	{�B	{�B	{�B	|�B	~�B	~�B	{�B	y�B	x�B	{�B	}�B	}�B	~�B	~�B	}�B	|�B	|�B	�B	�B	�B	�B	�B	�%B	�+B	�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
1'B
1'B
1'B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
.B
6FB
;dB
6FB
/B
!�B
�B
DB
$�B
$�B
bB
�B
<jB
bNB
m�B
q�B
|�B
�7B
��B
��B
�LB
��B
�ZB
��B
��B	7B,B<jBB�B]/Bl�Bp�Br�B~�B�DB��B�9B�B��B  B
=B0!BQ�Be`Bu�Bt�B|�B�B�=B��B��B��B�B�'B��B�B�LB�!B��B�LB�FB�B�B�!B�7BdZBS�BG�B49B�B�sB��B�%BW
BP�BQ�BI�BC�B(�B
��B
�BJB�BuBB
�HB
�B
{�B
ffB
>wB
�B
uB	��B	��B	��B	z�B	�uB	��B	�B	gmB	XB	I�B	-B	uB	{B	PB��B�BB�BB�B��B��BǮB��B�dB�LB�?B�-B�B�B�B�qB�dB�^B�qB�jB�?B�B�B��B��B��B�B�-B�'B�'B�!B�B�-B�?B�9B�B��B�B�-B�LB�qB��BÖB��B��B��B��B��B�B�B�/B�)B�#B�mB�B�B�yB�sB�yB��B	bB	{B	oB	VB	DB	PB	oB	�B	�B	{B	bB	1B��B��B��B�B�5B�B�B�5B�BB�5B�
B��B��B��B�B�B�B�
B�B�
B�B��B��B��B�
B�B�)B�;B�sB��B��B	B	B	DB		7B		7B	
=B	PB	�B	�B	�B	�B	�B	�B	�B	�B	!�B	(�B	'�B	)�B	+B	&�B	0!B	?}B	L�B	M�B	O�B	P�B	J�B	F�B	R�B	^5B	^5B	\)B	[#B	[#B	dZB	gmB	gmB	ffB	dZB	cTB	dZB	gmB	gmB	hsB	n�B	p�B	s�B	t�B	v�B	x�B	z�B	{�B	{�B	{�B	{�B	{�B	|�B	~�B	~�B	{�B	y�B	x�B	{�B	}�B	}�B	~�B	~�B	}�B	|�B	|�B	�B	�B	�B	�B	�B	�%B	�+B	�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190518                              AO  ARCAADJP                                                                    20181005190518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190518  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190518  QCF$                G�O�G�O�G�O�8000            