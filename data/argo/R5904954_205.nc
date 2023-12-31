CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:36Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @P   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Q�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  20181005191736  20181005191736  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @���;�Z1   @��Y OY@5�XbM��d�t�j~�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33B�  B�  B�  B�  B�  B���C�fC  C  C  C
  C  C  C�fC�fC  C  C  C  C  C  C   C"  C#�fC%�fC(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Ca�fCd  Cf  Ch  Ci�fCl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C}�fC�  C��3C��3C��3C�  C�  C��3C��3C�  C��C��C��C�  C�  C�  C��3C��3C�  C��C�  C��3C��3C��3C��3C��3C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C��3C��3C�  C��3C��fC�  C��C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C��C�  C�  C�  C�  C��C�  C��C��3C�  C�  C��3C�  C��3C��3C�  C�  C��3C��3C��3C��3C�  C��C��C��C��C��C��3C�  C��C�  C�  C��C�  C��C��C�  C�  C��C�  C�  C��C�  C�  C�  C��C�  C�  C�  C��C��C��C��C�  C��C�  C��3C�  C��3C�  C��C�  D   D �fDfD�fD��D� DfD�fDfD�fDfD�fD��D�fD  Dy�D  D� D��D	� D
fD
�fD�D� D  Dy�D�3Dy�D  D� D  D� D��D� D  D� D��Dy�D  D� DfD�fDfD�fDfD� D  Dy�D��D#��D$y�D%fD%� D%��D&� D'  D'� D'��D(� D)  D)� D*  D*� D+  D+�fD,fD,�fD-fD-� D-��D.� D/  D/� D/�3D0y�D1  D1� D2  D2y�D3  D3�fD4  D4y�D4�3D5y�D6  D6y�D7fD7� D8fD8�fD9fD9y�D:  D:�fD;  D;�fD<�D<�fD=  D=y�D>  D>� D>��D?� D@fD@�fDA  DAy�DBfDB� DCfDC��DD  DD� DE  DE� DF  DF�fDG  DG� DG��DHy�DI  DI� DJ  DJ� DJ��DK� DL  DL�fDMfDM� DN  DN�fDN�3DOy�DPfDP�fDP��DQ�fDQ��DR� DR��DS� DTfDT� DU  DU� DVfDV� DW  DW� DW��DX�fDX��DY� DZ  DZ� D[  D[� D\  D\� D]  D]y�D]��D^� D^��D_�fD`  D`y�DafDay�Da�3Db� Dc  Dc� DdfDdy�De  De� Df  Df� Dg  Dy��D�AH11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�\)@���@�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B7�RB@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�B�B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B��)B��)B�\B�B�B�\B�\B�\B�\B�\B��)C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C#�C%�C(�C*�C,�C.�C0�C1�C4�C6�C8�C:�C<!HC>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CW�CZ�C\�C^�C`�Ca�Cd�Cf�Ch�Ci�Cl�Cn!HCp�Cr�Ct�Cv�Cx�Cz�C|�C}�C��C��
C��
C��
C��C��C��
C��
C��C��C��C��C��C��C��C��
C��
C��C��C��C��
C��
C��
C��
C��
C��C��C��C��C��C��
C��C��C��C��C��
C��C��C��C�qC��C��
C��
C��
C��C��
C��=C��C��C��C��C��
C��C��C��
C��C��C��C��C��C��C��C��
C��C��
C��C��C��C��C��C��C��C��C��C��
C��C��C��
C��C��
C��
C��C��C��
C��
C��
C��
C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�qC��C��C��C��C�qC��C��C��C��C��C��
C��C��
C��C��C��D �D �RDRD�RD��D��DRD�RDRD�RDRD�RD��D�RD�D{�D�D��D��D	��D
RD
�RD�D��D�D{�D�D{�D�D��D�D��D��D��D�D��D��D{�D�D��DRD�RDRD�RDRD��D�D{�D��D#��D${�D%RD%��D%��D&��D'�D'��D'��D(��D)�D)��D*�D*��D+�D+�RD,RD,�RD-RD-��D-��D.��D/�D/��D/�D0{�D1�D1��D2�D2{�D3�D3�RD4�D4{�D4�D5{�D6�D6{�D7RD7��D8RD8�RD9RD9{�D:�D:�RD;�D;�RD<�D<�RD=�D={�D>�D>��D>��D?��D@RD@�RDA�DA{�DBRDB��DCRDC��DD�DD��DE�DE��DF�DF�RDG�DG��DG��DH{�DI�DI��DJ�DJ��DJ��DK��DL�DL�RDMRDM��DN�DN�RDN�DO{�DPRDP�RDP��DQ�RDQ��DR��DR��DS��DTRDT��DU�DU��DVRDV��DW�DW��DW��DX�RDX��DY��DZ�DZ��D[�D[��D\�D\��D]�D]{�D]��D^��D^��D_�RD`�D`{�DaRDa{�Da�Db��Dc�Dc��DdRDd{�De�De��Df�Df��Dg�Dy��D�B>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA��A�&�A�(�A�+A�(�A�(�A�+A�+A�+A�-A�-A�-A�/A�/A�/A�-A�+A�1'A�33A�1'A�1'A�+A�(�A��A��A� �A�&�A�(�A�-A�-A�/A�/A�-A�-A�&�A�&�A�$�A��A�oA�oA�VA�JA�JA�%A���A��A��mA��mA��`A��/A���A�l�A�l�A�\)A�+A���A�=qA�JA��A���A��FA��uA���A�
=A��A��9A�VA�v�A��A��A��RA�t�A���A�\)A��A���A��7A�&�A���A���A��\A�z�A�$�A���A�  A�9XA��hA��uA��yA���A���A�ZA�S�A�Q�A�A�A��A��TA�z�A�%A�E�A�^5A���A���A��`A��yA��mA�|�A�dZA�$�A��A��
A�VA���A���A�7LA��DA��A�%A��A�$�A��jA�x�A�33A���A��uA�~�A��wA�(�A��A+A}G�A{�;Ay�Av��At�yAs��As\)Ar��Ap��Ann�Ak��AhQ�Ag;dAe�TAdQ�Aa��A]33AZZAY�PAX1'AW;dAV$�ASS�AP�/AO�AOG�AN^5AL�+AI�#AHVAEO�AB�uAA�AA�-AA�7A@�/A>5?A<�A:�A9�A8�A7x�A6��A6��A5��A3ƨA3S�A3VA21'A1A/�A/��A/K�A.��A.9XA-��A,�!A+�A+7LA*z�A)��A(ȴA'�A%��A%"�A$�A$1A#l�A#C�A#
=A!�A �\A I�AA�A�PA�DA�HA��A��A%A=qA �AbA�hAVA�jA�;A�A�uA��A�
A$�A&�A��A�mA�A�DA�A�A
5?A	�^A	XA�`A�A�uA�-A�!A��A�wAS�A5?A�A j@���@���@���@���@��!@�hs@�ff@�\)@�@�D@�ƨ@�S�@�7@�Ĝ@�C�@���@��@�F@�t�@��@◍@�p�@ߝ�@�E�@�b@��#@ج@��
@׍P@�dZ@���@ղ-@�ƨ@���@�^5@���@���@�\)@�M�@�$�@�p�@���@���@ǶF@���@�|�@�@��m@î@�S�@��y@��@�@�V@��@�-@�V@��#@�O�@�z�@�Z@�dZ@��
@�(�@�(�@��
@���@���@�t�@�33@���@�v�@�=q@��T@�x�@�x�@���@���@��j@��j@��@��@�{@���@�`B@�p�@�J@�^5@���@�V@��`@���@�Ĝ@���@���@��
@��m@���@��F@�"�@�@��H@���@��!@���@�^5@���@�@��^@���@���@���@���@��u@�(�@�t�@�K�@�;d@�C�@��@�@���@���@�ff@��T@��7@�/@��@��u@��@��w@�
=@���@�~�@�^5@�-@�$�@�$�@��@��#@��-@�hs@�/@���@��`@���@��j@��w@�K�@�33@�+@��@��y@���@��@��7@�/@�j@��
@�ƨ@��w@��w@���@���@�;d@�~�@�V@�{@��#@���@��7@�7L@�V@���@��@��/@��j@���@��D@�z�@�bN@�Q�@�1'@�(�@�1@�ƨ@�\)@��P@�v�@��-@��7@��@�/@�t�@{��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�VA��A�&�A�(�A�+A�(�A�(�A�+A�+A�+A�-A�-A�-A�/A�/A�/A�-A�+A�1'A�33A�1'A�1'A�+A�(�A��A��A� �A�&�A�(�A�-A�-A�/A�/A�-A�-A�&�A�&�A�$�A��A�oA�oA�VA�JA�JA�%A���A��A��mA��mA��`A��/A���A�l�A�l�A�\)A�+A���A�=qA�JA��A���A��FA��uA���A�
=A��A��9A�VA�v�A��A��A��RA�t�A���A�\)A��A���A��7A�&�A���A���A��\A�z�A�$�A���A�  A�9XA��hA��uA��yA���A���A�ZA�S�A�Q�A�A�A��A��TA�z�A�%A�E�A�^5A���A���A��`A��yA��mA�|�A�dZA�$�A��A��
A�VA���A���A�7LA��DA��A�%A��A�$�A��jA�x�A�33A���A��uA�~�A��wA�(�A��A+A}G�A{�;Ay�Av��At�yAs��As\)Ar��Ap��Ann�Ak��AhQ�Ag;dAe�TAdQ�Aa��A]33AZZAY�PAX1'AW;dAV$�ASS�AP�/AO�AOG�AN^5AL�+AI�#AHVAEO�AB�uAA�AA�-AA�7A@�/A>5?A<�A:�A9�A8�A7x�A6��A6��A5��A3ƨA3S�A3VA21'A1A/�A/��A/K�A.��A.9XA-��A,�!A+�A+7LA*z�A)��A(ȴA'�A%��A%"�A$�A$1A#l�A#C�A#
=A!�A �\A I�AA�A�PA�DA�HA��A��A%A=qA �AbA�hAVA�jA�;A�A�uA��A�
A$�A&�A��A�mA�A�DA�A�A
5?A	�^A	XA�`A�A�uA�-A�!A��A�wAS�A5?A�A j@���@���@���@���@��!@�hs@�ff@�\)@�@�D@�ƨ@�S�@�7@�Ĝ@�C�@���@��@�F@�t�@��@◍@�p�@ߝ�@�E�@�b@��#@ج@��
@׍P@�dZ@���@ղ-@�ƨ@���@�^5@���@���@�\)@�M�@�$�@�p�@���@���@ǶF@���@�|�@�@��m@î@�S�@��y@��@�@�V@��@�-@�V@��#@�O�@�z�@�Z@�dZ@��
@�(�@�(�@��
@���@���@�t�@�33@���@�v�@�=q@��T@�x�@�x�@���@���@��j@��j@��@��@�{@���@�`B@�p�@�J@�^5@���@�V@��`@���@�Ĝ@���@���@��
@��m@���@��F@�"�@�@��H@���@��!@���@�^5@���@�@��^@���@���@���@���@��u@�(�@�t�@�K�@�;d@�C�@��@�@���@���@�ff@��T@��7@�/@��@��u@��@��w@�
=@���@�~�@�^5@�-@�$�@�$�@��@��#@��-@�hs@�/@���@��`@���@��j@��w@�K�@�33@�+@��@��y@���@��@��7@�/@�j@��
@�ƨ@��w@��w@���@���@�;d@�~�@�V@�{@��#@���@��7@�7L@�V@���@��@��/@��j@���@��D@�z�@�bN@�Q�@�1'@�(�@�1@�ƨ@�\)@��P@�v�@��-@��7@��@�/@�t�@{��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBJBPBPB\B\B\B\B\BVB\BVB\B\B\BbBbBbBhBoBoBoBoBoBuB{B�B�B�B�B"�B$�B5?BS�BaHBs�By�B�B�B�B�7B��B��B�{B��B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�\B�\B�\B�oB��B�{B�bB�=B{�B^5BZBN�BH�BH�BG�BF�BC�BA�B=qB6FB,B{B��B�NB�B��B��B�}B�-B�B��B��Bt�BXBG�B.B1B
�TB
��B
ŢB
�RB
��B
��B
��B
��B
��B
��B
�oB
�7B
�%B
~�B
o�B
`BB
K�B
9XB
,B
&�B
%�B
#�B
�B
B	�B	�;B	�
B	��B	��B	�B	�oB	�B	z�B	r�B	l�B	e`B	ZB	K�B	D�B	A�B	;dB	0!B	!�B	�B	
=B��B��B��B��B��B�B�`B�;B�)B�#B�B�
B�B�B��B��B��B��BǮBŢBĜBÖB��B�}B�qB�^B�LB�?B�3B�B�B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�PB�=B�7B�bB�hB�uB�oB�{B��B��B��B��B��B��B�{B��B�{B�oB�PB�DB�7B�%B�B� B|�Bw�Bs�Bq�Br�Bp�Bl�BgmBdZBaHB`BB`BB^5B[#BW
BT�BQ�BP�BO�BS�BQ�BO�BL�BL�BN�BO�BQ�BQ�BR�BQ�BO�BN�BM�BN�BN�BN�BO�BQ�BR�BR�BT�BW
BZB\)B]/B_;B`BBbNBdZB_;B[#BZBbNBgmBgmBjBk�Bm�Bn�Bx�B�B�B�JB�uB�uB��B��B��B��B�B�'B�!B�B��B��B��B	B	B	
=B	\B	oB	�B	�B	�B	�B	�B	"�B	$�B	'�B	/B	2-B	7LB	7LB	;dB	@�B	B�B	@�B	=qB	:^B	:^B	<jB	>wB	B�B	F�B	D�B	C�B	C�B	D�B	F�B	K�B	N�B	P�B	Q�B	R�B	T�B	XB	ZB	[#B	\)B	\)B	\)B	]/B	^5B	_;B	_;B	`BB	hsB	jB	iyB	jB	jB	l�B	m�B	o�B	q�B	s�B	s�B	t�B	t�B	u�B	v�B	v�B	w�B	y�B	}�B	� B	�B	�B	�1B	�=B	�DB	�JB	�JB	�JB	�PB	�VB	�\B	�bB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�3B	�FB	�FB	�LB	�^B	�dB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	��B	��B	B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B
j22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222BDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBDBJBPBPB\B\B\B\B\BVB\BVB\B\B\BbBbBbBhBoBoBoBoBoBuB{B�B�B�B�B"�B$�B5?BS�BaHBs�By�B�B�B�B�7B��B��B�{B��B�B�B��B��B��B��B��B��B��B��B��B��B�{B�oB�\B�\B�\B�\B�oB��B�{B�bB�=B{�B^5BZBN�BH�BH�BG�BF�BC�BA�B=qB6FB,B{B��B�NB�B��B��B�}B�-B�B��B��Bt�BXBG�B.B1B
�TB
��B
ŢB
�RB
��B
��B
��B
��B
��B
��B
�oB
�7B
�%B
~�B
o�B
`BB
K�B
9XB
,B
&�B
%�B
#�B
�B
B	�B	�;B	�
B	��B	��B	�B	�oB	�B	z�B	r�B	l�B	e`B	ZB	K�B	D�B	A�B	;dB	0!B	!�B	�B	
=B��B��B��B��B��B�B�`B�;B�)B�#B�B�
B�B�B��B��B��B��BǮBŢBĜBÖB��B�}B�qB�^B�LB�?B�3B�B�B��B��B��B��B��B��B��B��B�uB�hB�bB�\B�PB�=B�7B�bB�hB�uB�oB�{B��B��B��B��B��B��B�{B��B�{B�oB�PB�DB�7B�%B�B� B|�Bw�Bs�Bq�Br�Bp�Bl�BgmBdZBaHB`BB`BB^5B[#BW
BT�BQ�BP�BO�BS�BQ�BO�BL�BL�BN�BO�BQ�BQ�BR�BQ�BO�BN�BM�BN�BN�BN�BO�BQ�BR�BR�BT�BW
BZB\)B]/B_;B`BBbNBdZB_;B[#BZBbNBgmBgmBjBk�Bm�Bn�Bx�B�B�B�JB�uB�uB��B��B��B��B�B�'B�!B�B��B��B��B	B	B	
=B	\B	oB	�B	�B	�B	�B	�B	"�B	$�B	'�B	/B	2-B	7LB	7LB	;dB	@�B	B�B	@�B	=qB	:^B	:^B	<jB	>wB	B�B	F�B	D�B	C�B	C�B	D�B	F�B	K�B	N�B	P�B	Q�B	R�B	T�B	XB	ZB	[#B	\)B	\)B	\)B	]/B	^5B	_;B	_;B	`BB	hsB	jB	iyB	jB	jB	l�B	m�B	o�B	q�B	s�B	s�B	t�B	t�B	u�B	v�B	v�B	w�B	y�B	}�B	� B	�B	�B	�1B	�=B	�DB	�JB	�JB	�JB	�PB	�VB	�\B	�bB	�hB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�3B	�FB	�FB	�LB	�^B	�dB	�qB	�wB	�}B	�}B	��B	��B	��B	��B	��B	��B	B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B
j22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191736                              AO  ARCAADJP                                                                    20181005191736    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191736  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191736  QCF$                G�O�G�O�G�O�8000            