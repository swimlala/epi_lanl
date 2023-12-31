CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  0   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:03Z creation      
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
_FillValue                    u,Argo profile    3.1 1.2 19500101000000  20181005191703  20181005191703  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               BA   AO  6557                            2B  A   APEX                            7468                            062512                          846 @׿�	�k�1   @׿���@5]�E���d�hr�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      BA   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A���B ffB��B��B33B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C9�fC<  C>�C@�CB  CD  CF  CG�fCI�fCL  CN�CP  CR  CT�CV  CX  CZ  C\�C^  C`  Cb�Cd�Cf  Cg�fCj  Cl  Cm�fCp  Cr  Cs�fCu�fCw�fCz  C|  C}�fC�fC��3C�  C�  C�  C��C�  C�  C��C�  C�  C��C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��3C��fC��fC�  C�  C��C��C��3C��C�  C�  C��C�  C��fC�  C�  C�  C�  C�  C��C�  C��3C�  C��C��C�  C�  C��3C��3C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C��C�  C�  C��C�  C��3C�  C�  C��C��C�  C�  C��C�  C�  C��C�  C��3C��C��C��C��C�  C�  C��C�  C�  C�  C��3C��C�  C�  C�  C��3C�  C��C��C��C��3C��3C�  C��C��C��C�  C��C��C�  C�  C��3D   D � D  D� D��D� D��D� D  D� D��Dy�D  Dy�D��Dy�D  D� D��D	� D
  D
� D  D� D  D� D  D�fD  D� DfD� D�3Dy�D  D� DfD� D��Dy�D  Dy�D��D� D  D� D  Dy��D�<�D��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�
=@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A���A���Bz�B	�HB�BG�B!{B){B1{B9{BA{BI{BQ{BY{Ba{Bi{Bq{By{B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��=B��pB��pB��pB��=BĊ=BȊ=B̊=BЊ=B�W
B؊=B܊=B��=B�=B�=B�=B��=B�=B��pB��=C +�CECECECEC
ECECECECECECECECECECEC ^�C"EC$EC&EC(EC*EC,EC.EC0EC2^�C4EC6EC8EC:+�C<EC>^�C@^�CBECDECFECH+�CJ+�CLECN^�CPECRECT^�CVECXECZEC\^�C^EC`ECb^�Cd^�CfECh+�CjEClECn+�CpECrECt+�Cv+�Cx+�CzEC|EC~+�C��C��C�"�C�"�C�"�C�/\C�"�C�"�C�/\C�"�C�"�C�/\C�"�C�/\C�/\C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C��C��C��C�"�C�"�C�/\C�/\C��C�/\C�"�C�"�C�<)C�"�C��C�"�C�"�C�"�C�"�C�"�C�/\C�"�C��C�"�C�/\C�/\C�"�C�"�C��C��C�"�C�"�C�"�C��C��C��C��C��C�"�C�"�C�"�C�"�C�"�C�"�C��C�"�C�/\C�"�C��C�"�C�/\C�/\C�"�C�"�C�/\C�"�C��C�"�C�"�C�/\C�/\C�"�C�"�C�/\C�"�C�"�C�/\C�"�C��C�/\C�/\C�/\C�<)C�"�C�"�C�<)C�"�C�"�C�"�C��C�/\C�"�C�"�C�"�C��C�"�C�/\C�/\C�/\C��C��C�"�C�/\C�/\C�/\C�"�C�/\C�/\C�"�C�"�C��D HD �HDHD�HD
�D�HD
�D�HDHD�HD
�D��DHD��D
�D��DHD�HD	
�D	�HD
HD
�HDHD�HDHD�HDHD��DHD�HD�D�HD{D��DHD�HD�D�HD
�D��DHD��D
�D�HDHD�HDHDy��D�EqD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A��HA��A؝�A؛�A�l�A֏\A��A�{A�-AυA� �A�bNA�ȴA�~�A��`A�p�A�$�A��`AľwAĝ�A��A�5?A���A���A�/A���A���A��jA�bNA�A�~�A�9XA�M�A���A���A�ZA�dZA���A�v�A�I�A��PA�$�A��RA�E�A���A��A��A���A��A��-A��/A��A�  A�ƨA���A��9A�jA��A��A�ƨA��RA�JA�C�A��!A��^A��9A�bNA�`BA���A��-A�
=A���A��A���A��A��^A���A���A��hA�dZA��A�=qA���A���A���A�9XA�ȴA�M�A�A���A�9XA��;A���A�n�A���A���A��PA�+A|�`Ay�7AxjAv��As�Am�^Ak�-Ag��Ae;dAbffA^��A[;dAWl�AUp�AT^5AQ�
AP��AP��AO��AO"�AL�+AJ�AI�PAH �AF��AE+AD�RAC��AA��AA&�A?�hA>�A>z�A=/A<�DA;�wA:~�A9;dA7�A6I�A5|�A4=qA3&�A2A�A1��A1�#A.�9A+�FA*��A(��A(�A&��A%�A$�/A r�A�;AhsA��A-A�-A�`AAffAC�A9XA�^A�HA1'A{A�A��A��A�`AK�A�A�`A�+A��AS�A
n�A
$�A	�wA	XA	G�A	;dA��A�A�jAA��AA�A/A1'A�hA��AXA r�@��m@�S�@��@���@�x�@��!@���@�Ĝ@��j@�j@�F@�^5@��@�&�@�@���@���@�@��;@�~�@�-@���@���@�7L@��@�o@�=q@�?}@�%@���@��@���@�&�@�p�@���@�C�@�R@���@�x�@�V@�  @�dZ@�=q@�/@�1@��y@���@��T@Չ7@�%@�\)@��@мj@�A�@�~�@́@�%@���@̋D@ˮ@�l�@�+@��@�n�@ɡ�@���@��H@Ł@Ĭ@��@î@�C�@��@�o@���@\@�@�I�@��
@���@��@�p�@��h@�v�@���@�`B@�Ĝ@�I�@��m@�;d@��y@��H@�5?@��#@��O@~�@kS�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A���A���A���A��HA��A؝�A؛�A�l�A֏\A��A�{A�-AυA� �A�bNA�ȴA�~�A��`A�p�A�$�A��`AľwAĝ�A��A�5?A���A���A�/A���A���A��jA�bNA�A�~�A�9XA�M�A���A���A�ZA�dZA���A�v�A�I�A��PA�$�A��RA�E�A���A��A��A���A��A��-A��/A��A�  A�ƨA���A��9A�jA��A��A�ƨA��RA�JA�C�A��!A��^A��9A�bNA�`BA���A��-A�
=A���A��A���A��A��^A���A���A��hA�dZA��A�=qA���A���A���A�9XA�ȴA�M�A�A���A�9XA��;A���A�n�A���A���A��PA�+A|�`Ay�7AxjAv��As�Am�^Ak�-Ag��Ae;dAbffA^��A[;dAWl�AUp�AT^5AQ�
AP��AP��AO��AO"�AL�+AJ�AI�PAH �AF��AE+AD�RAC��AA��AA&�A?�hA>�A>z�A=/A<�DA;�wA:~�A9;dA7�A6I�A5|�A4=qA3&�A2A�A1��A1�#A.�9A+�FA*��A(��A(�A&��A%�A$�/A r�A�;AhsA��A-A�-A�`AAffAC�A9XA�^A�HA1'A{A�A��A��A�`AK�A�A�`A�+A��AS�A
n�A
$�A	�wA	XA	G�A	;dA��A�A�jAA��AA�A/A1'A�hA��AXA r�@��m@�S�@��@���@�x�@��!@���@�Ĝ@��j@�j@�F@�^5@��@�&�@�@���@���@�@��;@�~�@�-@���@���@�7L@��@�o@�=q@�?}@�%@���@��@���@�&�@�p�@���@�C�@�R@���@�x�@�V@�  @�dZ@�=q@�/@�1@��y@���@��T@Չ7@�%@�\)@��@мj@�A�@�~�@́@�%@���@̋D@ˮ@�l�@�+@��@�n�@ɡ�@���@��H@Ł@Ĭ@��@î@�C�@��@�o@���@\@�@�I�@��
@���@��@�p�@��h@�v�@���@�`B@�Ĝ@�I�@��m@�;d@��y@��H@�5?@��#@��O@~�@kS�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺBǮBB��B��B��B�B�ZB��BB&�B-B1'B49B7LB8RB8RB=qBH�B_;BjBu�B|�B�\B��B�{B�hB�PB�{B��B�LB��B��B��B��B�#B�
B��B��B��BB�qB�dB�^B�LB�LB�^B�RB�B��B�uB�%Bu�Bo�BiyBcTBZBJ�B33B�B��B�B�;B��B�!B��B��B��B�7Bs�BaHBR�BE�B7LB49B2-B49BO�BR�BS�BVBXBJ�B@�B7LB"�B\B1BB
��B
��B
�B
��B
�B
{�B
T�B
:^B
1'B
!�B
+B	�fB	�B	ÖB	�B	�{B	x�B	]/B	G�B	<jB	6FB	,B	%�B	#�B	�B	�B	bB		7B	B��B�B�B�B�mB�BB�)B�
B��B��B��B��B��BǮB��B�^B�RB�RB�XB�jB�wB�qB�^B�?B�'B�B�B��B��B��B��B�DB{�Bx�Bw�Bu�Bs�Bp�Bq�By�B~�Bz�By�Bs�Bn�Bl�BjBgmBcTB\)BYBXBW
BVBS�BM�BI�BH�BG�BF�BK�BYBQ�BG�BC�BP�B\)BffBffBe`BgmBffBdZBe`BffBgmBhsBgmBiyBm�Bx�B{�Bz�B{�B{�B|�B|�B|�B{�B}�B� B�B�B�B�JB�PB�\B�uB��B��B�{B�hB�uB�uB��B��B��B��B��B��B��B�B�!B�!B�9B�?B�FB�LB�RB�XB�dB��B��B��BĜBɺB��B��B��B��B�
B�)B�5B�HB�TB�ZB�`B�fB�sB�yB�B�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	
=B	DB	
=B	
=B	
=B		7B	PB	{B	{B	�B	�B
B
B
&L2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺBɺBǮBB��B��B��B�B�ZB��BB&�B-B1'B49B7LB8RB8RB=qBH�B_;BjBu�B|�B�\B��B�{B�hB�PB�{B��B�LB��B��B��B��B�#B�
B��B��B��BB�qB�dB�^B�LB�LB�^B�RB�B��B�uB�%Bu�Bo�BiyBcTBZBJ�B33B�B��B�B�;B��B�!B��B��B��B�7Bs�BaHBR�BE�B7LB49B2-B49BO�BR�BS�BVBXBJ�B@�B7LB"�B\B1BB
��B
��B
�B
��B
�B
{�B
T�B
:^B
1'B
!�B
+B	�fB	�B	ÖB	�B	�{B	x�B	]/B	G�B	<jB	6FB	,B	%�B	#�B	�B	�B	bB		7B	B��B�B�B�B�mB�BB�)B�
B��B��B��B��B��BǮB��B�^B�RB�RB�XB�jB�wB�qB�^B�?B�'B�B�B��B��B��B��B�DB{�Bx�Bw�Bu�Bs�Bp�Bq�By�B~�Bz�By�Bs�Bn�Bl�BjBgmBcTB\)BYBXBW
BVBS�BM�BI�BH�BG�BF�BK�BYBQ�BG�BC�BP�B\)BffBffBe`BgmBffBdZBe`BffBgmBhsBgmBiyBm�Bx�B{�Bz�B{�B{�B|�B|�B|�B{�B}�B� B�B�B�B�JB�PB�\B�uB��B��B�{B�hB�uB�uB��B��B��B��B��B��B��B�B�!B�!B�9B�?B�FB�LB�RB�XB�dB��B��B��BĜBɺB��B��B��B��B�
B�)B�5B�HB�TB�ZB�`B�fB�sB�yB�B�sB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B	
=B	DB	
=B	
=B	
=B		7B	PB	{B	{B	�B	�B
B
B
&L2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191703                              AO  ARCAADJP                                                                    20181005191703    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191703  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191703  QCF$                G�O�G�O�G�O�8000            