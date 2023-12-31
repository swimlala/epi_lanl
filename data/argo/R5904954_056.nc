CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  0   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:01Z creation      
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
_FillValue                 0  >0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ?`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  D    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  EP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  J   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  N�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  T�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  _p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  `�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 0  e`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  kP   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    k�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    n�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    q�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  t�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    t�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    t�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    t�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    t�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  t�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    t�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    u   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    u   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         u    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         u$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        u(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    u,Argo profile    3.1 1.2 19500101000000  20181005191701  20181005191701  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               8A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׽d����1   @׽e}'�6@5["��`B�c�Z�11   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      8A   A   A   @���@���A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C!�fC$  C&  C(  C*  C,  C.�C0  C1�fC3�fC6  C8  C:�C<  C>�C@�CB  CC�fCF  CH�CJ�CL  CN  CO�fCR  CT  CV  CX  CZ  C\�C^�C`  Cb�Cd  Cf  Cg�fCi�fCk�fCn�Cp  Cr  Cs�fCv  Cx�Cz  C|  C}�fC�fC�  C��3C��3C��3C��3C�  C��3C��C��C�  C�  C�  C��C��C��3C��3C�  C��3C�  C��3C�  C�  C��C��C�  C��3C��C��C�  C�  C��3C��3C�  C��C��3C��3C�  C�  C��C��C�  C�  C��C�  C��C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C��3C��3C��3C��3C�  C��3C�  C��C��3C�  C�  C�  C��C��C��C�  C��3C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C��3C�  C�  C��3C�  D fD �fDfD� D  D�fD  Dy�D��Dy�D��D� D��D� DfD�fDfD� D	  D	� D	��D
� D  D� DfD�fD  D� D  Dy�D��Dy�D  D�fDfD�fD  D� D  D� DfD� D  D�fDfD� D��Dy��D�)�D��
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�{@�G�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B((�B0�\B8�\B@�\BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�z�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�{B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C=qC#�C #�C"
=C$#�C&#�C(#�C*#�C,#�C.=qC0#�C2
=C4
=C6#�C8#�C:=qC<#�C>=qC@=qCB#�CD
=CF#�CH=qCJ=qCL#�CN#�CP
=CR#�CT#�CV#�CX#�CZ#�C\=qC^=qC`#�Cb=qCd#�Cf#�Ch
=Cj
=Cl
=Cn=qCp#�Cr#�Ct
=Cv#�Cx=qCz#�C|#�C~
=C�C��C�C�C�C�C��C�C��C��C��C��C��C��C��C�C�C��C�C��C�C��C��C��C��C��C�C��C�+�C��C��C�C�C��C��C�C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C�C�C�C��C��C��C�C��C��C��C��C��C�C�C�C�C�C��C�C��C��C�C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C�C��C�C��C��C�C��D \D �\D\D��D�D�\D�D��D�D��D�D��D�D��D\D�\D\D��D	�D	��D
�D
��D�D��D\D�\D�D��D�D��D�D��D�D�\D\D�\D�D��D�D��D\D��D�D�\D\D��D�Dy��D�.D�˅1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�A�%A�1A�A�%A���A��mAز-A�\)AҴ9A��AѓuA�1A��yA��/AГuA�K�A�(�A�v�A�XA��A͍PA�JA�$�A�ffA�1'AǗ�A�l�A�ZA�A��A�A�=qA��mA���A���A�hsA��;A�|�A��A�VA��yA�A�A�bNA���A���A�r�A���A��jA��A�E�A�1A��RA�~�A�/A���A���A��A�l�A�1'A��mA��A��!A��A�7LA��wA��yA�K�A�l�A�I�A�7LA�jA��wA�?}A���A�r�A��A�\)A�hsA�{A�dZA��A���A��+A�ȴA���A���A�"�A�ffA�&�A�/A��wA��A��A�7LA�A}�PAz��AyhsAv�uApI�Am��AlVAlAk`BAix�Ad �Aa�wA_O�A]��A[�AY��AW�-AU��AT�`AT�uAQ?}AMK�AL  AK/AK%AJ��AI�AI+AH�+AHA�AG�AGhsAF�uAEx�AD �ABJA@jA>��A=�A<ZA;��A;�A:�DA:JA7�A3/A2bA0�yA0^5A/�A.VA-/A,{A*jA(�A'%A%��A%\)A$�yA$1'A#S�A"ȴA ��A�A1'Ax�A�+A`BAXAVA1A��A�TA�^A�PAdZA�AA�/AVA��A?}AbNA�A�At�A?}A�AE�A�wA�-Ax�AA"�A
�uA
1A	;dAȴA��AE�AS�A33A��A5?AXAn�Ap�AoA �!@���@�Z@�ƨ@�S�@��@�o@���@��^@�b@�t�@�K�@�E�@�@�z�@�{@��@�{@�dZ@�j@�-@�b@�
=@���@���@㕁@߾w@��T@݁@�&�@ڸR@��`@ץ�@�p�@�  @ӍP@��@��H@��T@�bN@υ@�S�@�~�@���@͉7@��/@�I�@�ƨ@�@�M�@���@���@�?}@�1@�;d@�@Ƨ�@�n�@�@�p�@�Ĝ@�bN@�K�@�~�@�@�p�@�?}@�?}@�Ĝ@���@��
@���@�|�@���@���@�`B@�&�@��F@��@�ȴ@�ff@���@��-@��7@�&�@��`@�A�@��w@���@���@��@�n�@�$�@�{@�{@�{@���@yB�@j�A1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�A�%A�1A�A�%A���A��mAز-A�\)AҴ9A��AѓuA�1A��yA��/AГuA�K�A�(�A�v�A�XA��A͍PA�JA�$�A�ffA�1'AǗ�A�l�A�ZA�A��A�A�=qA��mA���A���A�hsA��;A�|�A��A�VA��yA�A�A�bNA���A���A�r�A���A��jA��A�E�A�1A��RA�~�A�/A���A���A��A�l�A�1'A��mA��A��!A��A�7LA��wA��yA�K�A�l�A�I�A�7LA�jA��wA�?}A���A�r�A��A�\)A�hsA�{A�dZA��A���A��+A�ȴA���A���A�"�A�ffA�&�A�/A��wA��A��A�7LA�A}�PAz��AyhsAv�uApI�Am��AlVAlAk`BAix�Ad �Aa�wA_O�A]��A[�AY��AW�-AU��AT�`AT�uAQ?}AMK�AL  AK/AK%AJ��AI�AI+AH�+AHA�AG�AGhsAF�uAEx�AD �ABJA@jA>��A=�A<ZA;��A;�A:�DA:JA7�A3/A2bA0�yA0^5A/�A.VA-/A,{A*jA(�A'%A%��A%\)A$�yA$1'A#S�A"ȴA ��A�A1'Ax�A�+A`BAXAVA1A��A�TA�^A�PAdZA�AA�/AVA��A?}AbNA�A�At�A?}A�AE�A�wA�-Ax�AA"�A
�uA
1A	;dAȴA��AE�AS�A33A��A5?AXAn�Ap�AoA �!@���@�Z@�ƨ@�S�@��@�o@���@��^@�b@�t�@�K�@�E�@�@�z�@�{@��@�{@�dZ@�j@�-@�b@�
=@���@���@㕁@߾w@��T@݁@�&�@ڸR@��`@ץ�@�p�@�  @ӍP@��@��H@��T@�bN@υ@�S�@�~�@���@͉7@��/@�I�@�ƨ@�@�M�@���@���@�?}@�1@�;d@�@Ƨ�@�n�@�@�p�@�Ĝ@�bN@�K�@�~�@�@�p�@�?}@�?}@�Ĝ@���@��
@���@�|�@���@���@�`B@�&�@��F@��@�ȴ@�ff@���@��-@��7@�&�@��`@�A�@��w@���@���@��@�n�@�$�@�{@�{@�{@���@yB�@j�A1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bv�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bx�B�B��BǮBĜB�dB�RB�RB�RB�LB�FB�3B�!B�?BÖB��B��B�B�B	7B�B2-BL�B]/BcTBx�B}�B� B�B�B�DB�VB�uB��B��B�B�B��B��B�B�NB�HB�HB�;B�;B�;B�HB�`B�yB�B�B�B�B�B�B�ZB�HB�5B�B��BɺB��B��B�\B�B|�Bw�Be`B49B.B%�B�B�BuBB�B�TB�
B�RB��BiyBC�B�B
�B
�B
ɺB
�3B
��B
bNB
O�B
<jB
0!B
�B	�B	�/B	��B	��B	ȴB	�LB	��B	�+B	x�B	l�B	]/B	S�B	G�B	>wB	:^B	6FB	$�B	�B	hB	hB	�B	�B	oB	�B	$�B	+B	+B	'�B	#�B	�B	�B	JB	%B��B��B��B�B�B�B�ZB��B�wB�RB�3B�!B�B��B��B��B�{B�PB�JB�JB�DB�DB�7B�+B�B�B{�Bw�Bu�Bs�Bq�Bn�Bo�Bo�Bo�Bp�Bp�Bq�Br�Bs�Bv�Bz�B�B�7B�JB�=B�DB�uB��B��B��B��B�B�FB�FB�XB�RB�^B��B�wB�wB�qB�qB�dB�^B�^B�^B�jB�RB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�9B�wB�wB�RB�B��B�B�B�B��B��B��B��B��B�B�9B�-B�3B�^B�}B��BŢBǮB��B��B��B��B�B�#B�)B�/B�;B�;B�;B�NB�TB�ZB�`B�B�B�B�B�B�B��B��B��B��B	  B	B	B	%B	%B	+B	
=B	
=B	
=B	
=B	DB	PB	hB	uB	�B	�B	�B	�B	�B	 �B	"�B	#�B	#�B	%�B	'�B	)�B	-B	1'B	2-B	33B	49B	6FB	7LB

rB
WB
'�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222Bv�Bv�Bv�Bv�Bv�Bv�Bw�Bw�Bx�B�B��BǮBĜB�dB�RB�RB�RB�LB�FB�3B�!B�?BÖB��B��B�B�B	7B�B2-BL�B]/BcTBx�B}�B� B�B�B�DB�VB�uB��B��B�B�B��B��B�B�NB�HB�HB�;B�;B�;B�HB�`B�yB�B�B�B�B�B�B�ZB�HB�5B�B��BɺB��B��B�\B�B|�Bw�Be`B49B.B%�B�B�BuBB�B�TB�
B�RB��BiyBC�B�B
�B
�B
ɺB
�3B
��B
bNB
O�B
<jB
0!B
�B	�B	�/B	��B	��B	ȴB	�LB	��B	�+B	x�B	l�B	]/B	S�B	G�B	>wB	:^B	6FB	$�B	�B	hB	hB	�B	�B	oB	�B	$�B	+B	+B	'�B	#�B	�B	�B	JB	%B��B��B��B�B�B�B�ZB��B�wB�RB�3B�!B�B��B��B��B�{B�PB�JB�JB�DB�DB�7B�+B�B�B{�Bw�Bu�Bs�Bq�Bn�Bo�Bo�Bo�Bp�Bp�Bq�Br�Bs�Bv�Bz�B�B�7B�JB�=B�DB�uB��B��B��B��B�B�FB�FB�XB�RB�^B��B�wB�wB�qB�qB�dB�^B�^B�^B�jB�RB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B�B��B��B�9B�wB�wB�RB�B��B�B�B�B��B��B��B��B��B�B�9B�-B�3B�^B�}B��BŢBǮB��B��B��B��B�B�#B�)B�/B�;B�;B�;B�NB�TB�ZB�`B�B�B�B�B�B�B��B��B��B��B	  B	B	B	%B	%B	+B	
=B	
=B	
=B	
=B	DB	PB	hB	uB	�B	�B	�B	�B	�B	 �B	"�B	#�B	#�B	%�B	'�B	)�B	-B	1'B	2-B	33B	49B	6FB	7LB

rB
WB
'�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191701                              AO  ARCAADJP                                                                    20181005191701    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191701  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191701  QCF$                G�O�G�O�G�O�8000            