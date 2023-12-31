CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  1   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:02Z creation      
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
_FillValue                    uhArgo profile    3.1 1.2 19500101000000  20181005191702  20181005191702  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               <A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׾d���1   @׾e>���@5a�7Kƨ�c��Q�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      <A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A���A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C3�fC5�fC7�fC:  C<  C>  C@  CB  CD  CE�fCH  CJ�CL  CN  CP  CQ�fCT  CV  CW�fCY�fC\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cm�fCo�fCr  Ct  Cv  Cx  Cy�fC|�C~  C�fC�  C�  C��3C�  C�  C��3C��3C��3C�  C��C��C��C�  C��3C�  C�  C��3C��3C�  C��C�  C��3C��3C��C�  C��3C�  C��C��C��C��3C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C��C��3C��3C��3C�  C��C�  C��3C��3C��fC�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C��3C�  C�  C��fC��3C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��3C�  C��C�  C�  C�  C�  C��3C�  C��C��C��C��C��C�  C��3C�  C��3C�  C��C�  C�  C�  C��3C��3C��3C��C�  C��3C��3C�  C�  C��3C��3C��3C��3C��3C�  D fD � DfD� D��Dy�D��D� D  Dy�D��Dy�D��D� D  Dy�D��Dy�D	  D	� D	��D
�fD  Dy�D��Dy�D  D� D  D�fD  Dy�D��Dy�D  D�fD��D� D  Dy�D  D� D  D� D  D� D  Dy��D�FD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @HQ�@��\@ʏ\AG�A%G�AEG�AeG�A���A���A�p�A���A£�A�p�A��A��BQ�B	Q�BQ�BQ�B!Q�B)Q�B1Q�B9Q�BAQ�BIQ�BQQ�BYQ�BaQ�BiQ�BqQ�ByQ�B���B���B���B���B���B���B���B���B���B���B���B���B��)B���B���B���B���BĨ�BȨ�B̨�BШ�BԨ�Bب�Bܨ�B��B��B��B��B��B���B���B�u�C :�CT{CT{CT{CT{C
T{CT{CT{CnCT{CT{CT{CT{CT{CT{CT{C T{C"T{C$T{C&:�C(T{C*T{C,T{C.T{C0T{C2T{C4:�C6:�C8:�C:T{C<T{C>T{C@T{CBT{CDT{CF:�CHT{CJnCLT{CNT{CPT{CR:�CTT{CVT{CX:�CZ:�C\T{C^T{C`T{CbT{CdT{Cf:�ChT{CjT{ClT{Cn:�Cp:�CrT{CtT{CvT{CxT{Cz:�C|nC~T{C�pC�*=C�*=C�pC�*=C�*=C�pC�pC�pC�*=C�7
C�7
C�7
C�*=C�pC�*=C�*=C�pC�pC�*=C�7
C�*=C�pC�pC�7
C�*=C�pC�*=C�C�C�7
C�7
C�pC�7
C�*=C�pC�*=C�*=C�*=C�*=C�*=C�*=C�*=C�*=C�pC�*=C�pC�*=C�*=C�7
C�pC�pC�pC�*=C�7
C�*=C�pC�pC��C�*=C�*=C�*=C�7
C�*=C�*=C�7
C�*=C�*=C�7
C�*=C�pC�*=C�*=C��C�pC�*=C�*=C�*=C�7
C�C�C�*=C�*=C�*=C�7
C�*=C�*=C�*=C�*=C�*=C�pC�*=C�pC�*=C�7
C�*=C�*=C�*=C�*=C�pC�*=C�7
C�7
C�7
C�7
C�7
C�*=C�pC�*=C�pC�*=C�7
C�*=C�*=C�*=C�pC�pC�pC�7
C�*=C�pC�pC�*=C�*=C�pC�pC�pC�pC�pC�*=D �D �D�D�D�D��D�D�DD��D�D��D�D�DD��D�D��D	D	�D
�D
��DD��D�D��DD�DD��DD��D�D��DD��D�D�DD��DD�DD�DD�DDy��D�P�D�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�  A�A���A�{A� �A�v�A�I�A�A�A�;dA�5?A�1'A�&�A��yA�AԲ-A�
=A��AҋDA�9XA�bNA�ĜA�l�A�`BA�1A��mAʍPA�ffA���A�VA�x�AƅA�oAŃA���A�r�AöFA¥�A��+A��mA�;dA���A�oA�z�A���A��PA��-A��!A�bA���A���A��;A�|�A��A�bNA���A�ȴA�XA���A�z�A�K�A�"�A��TA��A���A���A��TA�hsA��HA�A�A�ȴA�{A�9XA��A��!A��^A�oA�  A��
A��hA�Q�A��A��A��-A��A���A�K�A��DA��;A��A��A��hA���A�A���A�  A�JA~��A}�A|��Ay��Au�As/Arr�Aq�AqXAp5?An��AmG�Alz�Ak�;Ak�Aj(�Af�uA`��A]��A[oAZ(�AXv�AWt�AW?}AV�RAU�ATbNAR�`AO�AM�#AM��AMALbNAK��AI�AF��AD�AC��ACS�A@-A=��A<�A<r�A;��A:�A7ƨA5�FA4r�A2Q�A0^5A.I�A-A+��A*=qA)A)\)A(��A(JA'l�A&�yA%�A%�7A%&�A#��A"JA!oA�wAjA�HA1A&�AM�A�-A7LA(�A��A�hA7LA^5A�FAt�A�RA$�A��AM�A��AZA
I�AQ�A�wA�AdZA/A�A�+A5?A�7A"�A�A�/A�A�AI�A`BA �@�n�@�-@�@���@�/@���@�=q@���@�v�@�p�@�%@��9@�@�\@���@�V@�j@�ƨ@�=q@��@���@��T@��
@ꟾ@��@�p�@�b@�o@��`@ߕ�@�5?@ݡ�@ܣ�@��@�l�@�
=@ڇ+@��T@أ�@�1@ׅ@�33@֟�@���@ԛ�@�1@�+@���@ҟ�@�?}@Ͼw@·+@�x�@��@˅@�l�@�l�@�l�@�l�@�33@��@�@�ȴ@�ff@ȴ9@�Q�@��@Ƨ�@�5?@�J@��T@ũ�@Ł@��@�A�@��@��
@Å@�
=@�ff@�`B@��@��@�+@��@���@���@�{@�G�@�V@��j@�1@�~�@�J@��@��-@�G�@��`@���@��9@�K�@w��@g��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�  A�A���A�{A� �A�v�A�I�A�A�A�;dA�5?A�1'A�&�A��yA�AԲ-A�
=A��AҋDA�9XA�bNA�ĜA�l�A�`BA�1A��mAʍPA�ffA���A�VA�x�AƅA�oAŃA���A�r�AöFA¥�A��+A��mA�;dA���A�oA�z�A���A��PA��-A��!A�bA���A���A��;A�|�A��A�bNA���A�ȴA�XA���A�z�A�K�A�"�A��TA��A���A���A��TA�hsA��HA�A�A�ȴA�{A�9XA��A��!A��^A�oA�  A��
A��hA�Q�A��A��A��-A��A���A�K�A��DA��;A��A��A��hA���A�A���A�  A�JA~��A}�A|��Ay��Au�As/Arr�Aq�AqXAp5?An��AmG�Alz�Ak�;Ak�Aj(�Af�uA`��A]��A[oAZ(�AXv�AWt�AW?}AV�RAU�ATbNAR�`AO�AM�#AM��AMALbNAK��AI�AF��AD�AC��ACS�A@-A=��A<�A<r�A;��A:�A7ƨA5�FA4r�A2Q�A0^5A.I�A-A+��A*=qA)A)\)A(��A(JA'l�A&�yA%�A%�7A%&�A#��A"JA!oA�wAjA�HA1A&�AM�A�-A7LA(�A��A�hA7LA^5A�FAt�A�RA$�A��AM�A��AZA
I�AQ�A�wA�AdZA/A�A�+A5?A�7A"�A�A�/A�A�AI�A`BA �@�n�@�-@�@���@�/@���@�=q@���@�v�@�p�@�%@��9@�@�\@���@�V@�j@�ƨ@�=q@��@���@��T@��
@ꟾ@��@�p�@�b@�o@��`@ߕ�@�5?@ݡ�@ܣ�@��@�l�@�
=@ڇ+@��T@أ�@�1@ׅ@�33@֟�@���@ԛ�@�1@�+@���@ҟ�@�?}@Ͼw@·+@�x�@��@˅@�l�@�l�@�l�@�l�@�33@��@�@�ȴ@�ff@ȴ9@�Q�@��@Ƨ�@�5?@�J@��T@ũ�@Ł@��@�A�@��@��
@Å@�
=@�ff@�`B@��@��@�+@��@���@���@�{@�G�@�V@��j@�1@�~�@�J@��@��-@�G�@��`@���@��9@�K�@w��@g��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�oB�oB�hB�bB�hB�bB�PB�DB�JB�JB�PB�PB�PB�bB��B�!B�RBŢB��B��B��B��B��B��B�mB�B��B  B+B1B\B'�B/B8RBH�BVBffBw�B�B�hB��B�jB�BB�HB�TB�5B�#B�)B�/B�;B�NB�B��BŢB�dB�qB�wB�XB��B��B�%B|�Bz�Bu�BcTBP�BI�BD�B=qB33B �B��B�fB�
BÖB�'B�B�dB�^B�B��B�hB�PBt�B6FB�BoB
�B
�5B
ŢB
�'B
��B
��B
�VB
�1B
n�B
G�B
9XB
1'B
)�B
�B
B	��B	�B	�B	�mB	�BB	�
B	��B	��B	ɺB	ĜB	�jB	��B	u�B	^5B	M�B	J�B	A�B	<jB	;dB	8RB	2-B	+B	$�B	�B	�B	�B	{B	hB	hB	DB��B�B�B�`B��BɺBŢBB�}B�wB�RB�-B�B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B�uB�VB�7B�+B�B|�By�Bv�Bs�Bq�Bm�BffBbNB]/BZBYBXBW
BS�BP�BM�BK�BF�BH�BS�BYBZBZB[#B[#B]/B^5BaHBe`Bo�Bw�B}�B�B�1B�DB�1B�B�B�B�B�B� B�JB�hB�JB�+B�+B�7B�B�B�B�PB�hB��B��B��B��B��B�B�B�B�'B�-B�-B�9B�?B�LB�LB�RB�XB�XB�^B�^B�dB�wB�wB�}B�}B��BĜB��B��B��B��B��B��B��B�B�)B�NB�ZB�ZB�ZB�ZB�ZB�ZB�`B�`B�fB�yB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	+B	JB	\B	oB	uB	oB	�B	�B	$�B	'�B	(�B	)�B	-B	5?B	8RB	9XB	:^B	=qB	?}B	@�B	@�B
B
�B
*�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B�oB�oB�hB�bB�hB�bB�PB�DB�JB�JB�PB�PB�PB�bB��B�!B�RBŢB��B��B��B��B��B��B�mB�B��B  B+B1B\B'�B/B8RBH�BVBffBw�B�B�hB��B�jB�BB�HB�TB�5B�#B�)B�/B�;B�NB�B��BŢB�dB�qB�wB�XB��B��B�%B|�Bz�Bu�BcTBP�BI�BD�B=qB33B �B��B�fB�
BÖB�'B�B�dB�^B�B��B�hB�PBt�B6FB�BoB
�B
�5B
ŢB
�'B
��B
��B
�VB
�1B
n�B
G�B
9XB
1'B
)�B
�B
B	��B	�B	�B	�mB	�BB	�
B	��B	��B	ɺB	ĜB	�jB	��B	u�B	^5B	M�B	J�B	A�B	<jB	;dB	8RB	2-B	+B	$�B	�B	�B	�B	{B	hB	hB	DB��B�B�B�`B��BɺBŢBB�}B�wB�RB�-B�B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B�uB�VB�7B�+B�B|�By�Bv�Bs�Bq�Bm�BffBbNB]/BZBYBXBW
BS�BP�BM�BK�BF�BH�BS�BYBZBZB[#B[#B]/B^5BaHBe`Bo�Bw�B}�B�B�1B�DB�1B�B�B�B�B�B� B�JB�hB�JB�+B�+B�7B�B�B�B�PB�hB��B��B��B��B��B�B�B�B�'B�-B�-B�9B�?B�LB�LB�RB�XB�XB�^B�^B�dB�wB�wB�}B�}B��BĜB��B��B��B��B��B��B��B�B�)B�NB�ZB�ZB�ZB�ZB�ZB�ZB�`B�`B�fB�yB�B�B�B�B��B��B��B��B��B��B��B��B	  B	B	B	+B	JB	\B	oB	uB	oB	�B	�B	$�B	'�B	(�B	)�B	-B	5?B	8RB	9XB	:^B	=qB	?}B	@�B	@�B
B
�B
*�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.33 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191702                              AO  ARCAADJP                                                                    20181005191702    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191702  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191702  QCF$                G�O�G�O�G�O�8000            