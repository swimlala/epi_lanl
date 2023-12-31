CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  ?   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:20Z creation      
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
_FillValue                 @  >l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  E�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  J�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  O�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Q    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  V   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  W\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  \X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  aT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  g�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  m�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    m�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    p�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    s�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  v�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    w(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    w,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    w0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    w4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  w8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    wx   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    w�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    w�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         w�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         w�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        w�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    w�Argo profile    3.1 1.2 19500101000000  20181005190520  20181005190520  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               DA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��`���P1   @��aq�-@1�n��P�cw-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      DA   A   A   @�ff@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�33A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C��3C��3C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C��C��C��C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3C��3C��3C�  C�  C��3C��3D   D � D  D� D��Dy�D  D�fD  Dy�D  D� D  D� D  D�fD  Dy�D	  D	� D	��D
� D  D� D  D� D  D� D  Dy�D��D� DfD�fDfD� D  D� D  D� D��Dy�D��Dy�D��D� D  D� D  D� DfD� D  D�fDfD� D  D� D  D�fD  D� D  D� D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@�Q�A(�A$(�AEAd(�A�{A�{A�{A�{A�{A�G�A�{A�{B ��B	
=B
=B
=B!
=B)
=B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��RB��B��B�Q�B��B��B��RB��B��B��B��B��B��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�RB�B�B�Q�B�B��B��C \)CB�CB�CB�CB�C
B�CB�CB�CB�CB�CB�CB�CB�CB�CB�CB�C \)C"B�C$B�C&B�C(B�C*B�C,B�C.\)C0\)C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CFB�CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�Ct(�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�{C�{C�!HC�!HC�!HC�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�{C�!HC�{C�{C�!HC�!HC�!HC�.C�!HC�{C�{C�!HC�!HC�!HC�{C�!HC�!HC�!HC�{C�!HC�!HC�.C�.C�.C�!HC�{C�{C�!HC�!HC�!HC�{C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�!HC�!HC�{C�{C�{C�!HC�!HC�{C�{D �D ��D�D��D
>D�>D�D�
D�D�>D�D��D�D��D�D�
D�D�>D	�D	��D

>D
��D�D��D�D��D�D��D�D�>D
>D��D
D�
D
D��D�D��D�D��D
>D�>D
>D�>D
>D��D�D��D�D��D
D��D�D�
D
D��D�D��D�D�
D�D��D�D��D 
>1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�1A�oA�JA�VA�bA�oA�oA�oA�bA�bA�oA�oA�oA��A��A��A�"�A��A��A��A�;dAٸRA��A�&�A��A�Aٰ!A�JAؕ�A���A��A��A�ffA�bAӓuAѶFA�-A�  A��yA�l�A�ZA�ffA��
A�jA�Q�AđhA��A���A�9XA� �A��;A�A���A�~�A��A��A�-A��FA�K�A�oA���A��DA���A�(�A���A��!A�ffA���A��uA�G�A�l�A�33A��A��A��\A�C�A��7A�5?A�1'A��PA��A��/A��A��A���A�r�A��A��`A�ffA��/A�S�A�l�A��!A��yA���A��A��A���A�ȴA��HA��A���A�oA��A� �A�(�A�^5A��7A�dZA�%A�JA|AxQ�At�Ao��Aj�Ag�^Ac��A^ffA[XA[
=AY�-AX9XAW�AV�jAS"�APȴAPn�APVAO�FAO+ANn�AM/AL��AK
=AI�AGXAB�`AA��A?��A<�A:E�A8VA6Q�A5p�A4�+A2jA17LA.ffA-�A,��A,  A)�A)�A(�yA(�A%��A#��A"M�A!dZA!
=A ��A �Ax�A�jAffAhsA�A�A�A�A�uAG�AbNA;dA��A�A��AZA��An�A�#A`BA��A��A/A�yA�A��AK�A	�#Az�A��A�FA�9A��A�yA�HA�RA��A^5A�AG�A�A�w@��@��@�1'@��R@��@��@���@�1'@�ƨ@��R@��@���@�
=@��\@���@�w@�=q@�+@��y@��@�p�@�X@�?}@�O�@�7@�@陚@�h@��#@陚@蛦@�|�@�t�@�
=@���@�7L@��@���@�;d@◍@�$�@ᙚ@���@�@ۮ@��@��y@�n�@�G�@��@���@ۅ@�"�@��@��@�A�@ӶF@��@Ӯ@�ƨ@��;@�z�@ԣ�@�9X@�
=@�O�@�/@�7L@�?}@��@У�@��
@�t�@�S�@�;d@ΰ!@��#@�X@��@�+@��H@�~�@�-@���@�@���@���@���@ɺ^@ə�@�p�@�O�@�%@�9X@ư!@�hs@�C�@�@�@�v�@�ff@���@�l�@�V@�=q@�E�@�E�@�`B@��@���@��@�(�@��@�dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�1A�oA�JA�VA�bA�oA�oA�oA�bA�bA�oA�oA�oA��A��A��A�"�A��A��A��A�;dAٸRA��A�&�A��A�Aٰ!A�JAؕ�A���A��A��A�ffA�bAӓuAѶFA�-A�  A��yA�l�A�ZA�ffA��
A�jA�Q�AđhA��A���A�9XA� �A��;A�A���A�~�A��A��A�-A��FA�K�A�oA���A��DA���A�(�A���A��!A�ffA���A��uA�G�A�l�A�33A��A��A��\A�C�A��7A�5?A�1'A��PA��A��/A��A��A���A�r�A��A��`A�ffA��/A�S�A�l�A��!A��yA���A��A��A���A�ȴA��HA��A���A�oA��A� �A�(�A�^5A��7A�dZA�%A�JA|AxQ�At�Ao��Aj�Ag�^Ac��A^ffA[XA[
=AY�-AX9XAW�AV�jAS"�APȴAPn�APVAO�FAO+ANn�AM/AL��AK
=AI�AGXAB�`AA��A?��A<�A:E�A8VA6Q�A5p�A4�+A2jA17LA.ffA-�A,��A,  A)�A)�A(�yA(�A%��A#��A"M�A!dZA!
=A ��A �Ax�A�jAffAhsA�A�A�A�A�uAG�AbNA;dA��A�A��AZA��An�A�#A`BA��A��A/A�yA�A��AK�A	�#Az�A��A�FA�9A��A�yA�HA�RA��A^5A�AG�A�A�w@��@��@�1'@��R@��@��@���@�1'@�ƨ@��R@��@���@�
=@��\@���@�w@�=q@�+@��y@��@�p�@�X@�?}@�O�@�7@�@陚@�h@��#@陚@蛦@�|�@�t�@�
=@���@�7L@��@���@�;d@◍@�$�@ᙚ@���@�@ۮ@��@��y@�n�@�G�@��@���@ۅ@�"�@��@��@�A�@ӶF@��@Ӯ@�ƨ@��;@�z�@ԣ�@�9X@�
=@�O�@�/@�7L@�?}@��@У�@��
@�t�@�S�@�;d@ΰ!@��#@�X@��@�+@��H@�~�@�-@���@�@���@���@���@ɺ^@ə�@�p�@�O�@�%@�9X@ư!@�hs@�C�@�@�@�v�@�ff@���@�l�@�V@�=q@�E�@�E�@�`B@��@���@��@�(�@��@�dZ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�\B
�VB
�VB
�\B
�\B
�bB
�hB
�\B
�\B
�\B
��B
�B2-BN�BN�BN�BI�BA�B<jB49B1'B9XB:^B6FBE�B}�B��B�-B�-B��B�#B�HB�TB��B	7B�B)�B@�BF�BP�B]/Bn�B�B��B��B�XB�jBÖBǮBȴB��B��B��B�
B�B�B�B�/B�5B�5B�NB�TB�TB�TB�ZB�ZB�`B�ZB�fB�TB�5B�)B�B��B�jB�3B�B�9B��Bp�BK�B6FB �BhB	7BB�B�Bu�BH�B�BB
�B
�`B
�)B
�RB
��B
� B
aHB
O�B
B�B
�B	��B	�;B	�}B	��B	�=B	p�B	Q�B	H�B	G�B	A�B	;dB	7LB	49B	/B	&�B	$�B	#�B	�B	�B	�B	hB	PB	B��B�B�;B��B��BĜB�jB�LB�-B�B�B��B��B�B�qBÖB�jB�^B�dB�dB�LB�3B�RB�wB��B��B�}B�}B��B��B�wB�dB�}BÖBÖB��B��BȴBɺBĜB��B��BǮB��B��BȴBɺB��B��B�B�)B�)B�/B�5B�)B�#B�B��B�B��B��B��B��B��B��B��B�B�B�B�;B��B��B��BƨBȴBǮBƨBŢBɺB��B�B�ZB�yB�B��B�B�B�B�B�B�B�B��B��B	1B	PB	bB	�B	"�B	/B	1'B	1'B	33B	33B	33B	;dB	A�B	A�B	B�B	C�B	C�B	F�B	E�B	>wB	;dB	;dB	A�B	B�B	F�B	N�B	T�B	ZB	YB	N�B	I�B	G�B	H�B	O�B	T�B	XB	ZB	aHB	bNB	ffB	gmB	iyB	l�B	m�B	n�B	o�B	q�B	s�B	t�B	t�B	s�B	t�B	u�B	y�B	x�B	y�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�B	�B	|�B	~�B	� B	�B	�B	� B	}�B	~�B	~�B	~�B	~�B	�B	�B	�B	�%B	�+B	�1B	�+2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�\B
�VB
�VB
�\B
�\B
�bB
�hB
�\B
�\B
�\B
��B
�B2-BN�BN�BN�BI�BA�B<jB49B1'B9XB:^B6FBE�B}�B��B�-B�-B��B�#B�HB�TB��B	7B�B)�B@�BF�BP�B]/Bn�B�B��B��B�XB�jBÖBǮBȴB��B��B��B�
B�B�B�B�/B�5B�5B�NB�TB�TB�TB�ZB�ZB�`B�ZB�fB�TB�5B�)B�B��B�jB�3B�B�9B��Bp�BK�B6FB �BhB	7BB�B�Bu�BH�B�BB
�B
�`B
�)B
�RB
��B
� B
aHB
O�B
B�B
�B	��B	�;B	�}B	��B	�=B	p�B	Q�B	H�B	G�B	A�B	;dB	7LB	49B	/B	&�B	$�B	#�B	�B	�B	�B	hB	PB	B��B�B�;B��B��BĜB�jB�LB�-B�B�B��B��B�B�qBÖB�jB�^B�dB�dB�LB�3B�RB�wB��B��B�}B�}B��B��B�wB�dB�}BÖBÖB��B��BȴBɺBĜB��B��BǮB��B��BȴBɺB��B��B�B�)B�)B�/B�5B�)B�#B�B��B�B��B��B��B��B��B��B��B�B�B�B�;B��B��B��BƨBȴBǮBƨBŢBɺB��B�B�ZB�yB�B��B�B�B�B�B�B�B�B��B��B	1B	PB	bB	�B	"�B	/B	1'B	1'B	33B	33B	33B	;dB	A�B	A�B	B�B	C�B	C�B	F�B	E�B	>wB	;dB	;dB	A�B	B�B	F�B	N�B	T�B	ZB	YB	N�B	I�B	G�B	H�B	O�B	T�B	XB	ZB	aHB	bNB	ffB	gmB	iyB	l�B	m�B	n�B	o�B	q�B	s�B	t�B	t�B	s�B	t�B	u�B	y�B	x�B	y�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�%B	�B	�B	|�B	~�B	� B	�B	�B	� B	}�B	~�B	~�B	~�B	~�B	�B	�B	�B	�%B	�+B	�1B	�+2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.26 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190520                              AO  ARCAADJP                                                                    20181005190520    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190520  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190520  QCF$                G�O�G�O�G�O�8000            