CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:10Z creation      
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
resolution        =���   axis      Z        d  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   =�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  >�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   CT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  Dp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  H�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   M8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  NT   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   R�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  S�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  X8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   \�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ]�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   b   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  c8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  g�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    g�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    j�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    m�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  p�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    p�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    p�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    q    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    q   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  q   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    qH   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    qX   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    q\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ql   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         qp   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        qt   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    qxArgo profile    3.1 1.2 19500101000000  20181005191710  20181005191710  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               _A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$�_C�1   @��%Q��b@4�^5?}�d@Z�11   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      _A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B!��B'33B/��B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*�C,  C-�fC0  C1�fC4  C6  C8�C:  C;�fC=�fC@  CB�CD  CF  CH�CJ�CL  CN�CP�CR�CT  CV�CX�CZ  C\  C]�fC`  Cb  Cc�fCf  Ch  Ci�fCk�fCn  Cp  Cr  Ct33Cv�Cx33Cz33C|�C}�fC��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C��3C�  C��C��C��C��fC�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��C�  C��3C��3C�  C��C�  C��fC��fC��3C��3C�  C��C��C�  C��3C�  C�  C��3C�  C��C��C�  C�  C��C��C��C��C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C��C��C�  C��3C�  C��3C�  C�  C�  C�  C�  D   D � DfD�fD��D� DfD� DfD�fD  D� D  D�fD  Dy�D��D� D	  D	� D
fD
�fD  D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  Dy�)D�=qD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B!�B'�B/�B7�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�\)B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B���B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C.C{C{C{C {C"{C${C&{C({C*.C,{C-��C0{C1��C4{C6{C8.C:{C;��C=��C@{CB.CD{CF{CH.CJ.CL{CN.CP.CR.CT{CV.CX.CZ{C\{C]��C`{Cb{Cc��Cf{Ch{Ci��Ck��Cn{Cp{Cr{CtG�Cv.CxG�CzG�C|.C}��C�HC�
=C�
=C�
=C�
=C��pC�
=C�
=C�
=C�
=C�
C�
=C��pC�
=C�
C�
C�
C��C�
=C�
=C�
C�
=C�
=C�
=C��pC�
=C�
=C�
=C�
C�
=C��pC��pC�
=C�
C�
=C��C��C��pC��pC�
=C�
C�#�C�
=C��pC�
=C�
=C��pC�
=C�
C�
C�
=C�
=C�
C�
C�
C�
C�
C�
=C�
=C�
=C�
=C�
=C�
=C��pC�
=C�
=C�
=C�
=C�
=C�
C�
=C��pC�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�#�C�
C�
=C��pC�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C��pC�
=C�
C�#�C�
=C��pC�
=C��pC�
=C�
=C�
=C�
=C�
=D D �D�D��D��D�D�D�D�D��DD�DD��DD~�D��D�D	D	�D
�D
��DD�DD�D��D~�DD�DD�DD�DD�DD�DD~�D��D�DD�DD�DDy�HD�@ D�Å11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aݕ�Aݕ�Aݗ�Aݙ�Aݛ�Aݡ�Aݥ�Aݧ�AݬAݩ�Aݩ�AݬAݬAݰ!Aݲ-Aݣ�A݋DA݁A�ĜA�\)A��mAЙ�A�+AʋDA�`BA���A�7LAź^A�5?A��A��A�dZA� �A�t�A�v�A�&�A�"�A��+A���A���A���A��A��jA�C�A��jA��9A�&�A��uA���A��\A���A��A�ƨA���A��9A�I�A�ȴA��wA��A��+A�5?A���A�ȴA�hsA���A��A�7LA�O�A��A�|�A�5?A�%A�O�A���A�9XA��A�l�A�A�A��\A�oA��A�jA���A�JA��A�oA�x�A~VAz��Ax1'AwoAsC�Ap��Ao�7Ann�An9XAn�Am�wAl-Ajz�Ag�hAfbAd�DAahsA_\)A]�wA\ �AZffAW�-AW%AU��AT�jARĜAQ�PAO7LAK�mAJ�HAJZAI�-AHz�AE��AB-A@1'A>�/A>=qA=?}A:JA6M�A41A2n�A0��A/��A.�`A,�A+|�A*��A*{A)G�A)oA(��A(ȴA(Q�A'��A�TAK�A�A=qA�^Al�A�HA�mAA�A��AdZA"�A�A��AVA��A�A
�A
��A	ƨA	�AĜA �A+AVA�wAO�AĜAM�AbA`BA%Az�A�A �HA Z@��P@���@�7L@���@���@�x�@�bN@��!@�1'@�5?@���@�C�@�&�@�w@�~�@�&�@�9X@�
=@�hs@�J@�V@�Q�@�o@��@؛�@���@�
=@�E�@���@�\)@�{@Ͼw@Χ�@���@�X@���@̼j@̴9@��@�C�@�
=@��H@�M�@��@ɉ7@��@���@ź^@ź^@��@�n�@��@�ƨ@�(�@���@���@ǝ�@ŉ7@�V@��m@�v�@�E�@��@�7L@���@��w@���@�33@���@��y@���@��`@�V@�?}@��H@���@��@�Z@���@�O�@�/@�
=@�@�~�@���@�dZ@�S�@��P@��/@�"�@�v�@�~�@�V@�o@��9@��D@�Q�@�j@�K�@��c@l?�@Xی11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aݕ�Aݕ�Aݗ�Aݙ�Aݛ�Aݡ�Aݥ�Aݧ�AݬAݩ�Aݩ�AݬAݬAݰ!Aݲ-Aݣ�A݋DA݁A�ĜA�\)A��mAЙ�A�+AʋDA�`BA���A�7LAź^A�5?A��A��A�dZA� �A�t�A�v�A�&�A�"�A��+A���A���A���A��A��jA�C�A��jA��9A�&�A��uA���A��\A���A��A�ƨA���A��9A�I�A�ȴA��wA��A��+A�5?A���A�ȴA�hsA���A��A�7LA�O�A��A�|�A�5?A�%A�O�A���A�9XA��A�l�A�A�A��\A�oA��A�jA���A�JA��A�oA�x�A~VAz��Ax1'AwoAsC�Ap��Ao�7Ann�An9XAn�Am�wAl-Ajz�Ag�hAfbAd�DAahsA_\)A]�wA\ �AZffAW�-AW%AU��AT�jARĜAQ�PAO7LAK�mAJ�HAJZAI�-AHz�AE��AB-A@1'A>�/A>=qA=?}A:JA6M�A41A2n�A0��A/��A.�`A,�A+|�A*��A*{A)G�A)oA(��A(ȴA(Q�A'��A�TAK�A�A=qA�^Al�A�HA�mAA�A��AdZA"�A�A��AVA��A�A
�A
��A	ƨA	�AĜA �A+AVA�wAO�AĜAM�AbA`BA%Az�A�A �HA Z@��P@���@�7L@���@���@�x�@�bN@��!@�1'@�5?@���@�C�@�&�@�w@�~�@�&�@�9X@�
=@�hs@�J@�V@�Q�@�o@��@؛�@���@�
=@�E�@���@�\)@�{@Ͼw@Χ�@���@�X@���@̼j@̴9@��@�C�@�
=@��H@�M�@��@ɉ7@��@���@ź^@ź^@��@�n�@��@�ƨ@�(�@���@���@ǝ�@ŉ7@�V@��m@�v�@�E�@��@�7L@���@��w@���@�33@���@��y@���@��`@�V@�?}@��H@���@��@�Z@���@�O�@�/@�
=@�@�~�@���@�dZ@�S�@��P@��/@�"�@�v�@�~�@�V@�o@��9@��D@�Q�@�j@�K�@��c@l?�@Xی11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B.B.B.B.B.B.B-B-B-B-B-B-B-B-B-B,B)�B(�B �BDB+B	7B�B"�B)�B9XBH�BL�BQ�BT�B[#BaHBcTBiyBp�B}�B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�+Bx�BhsB]/BYBT�B@�B&�B�B��B�NB�
BǮB�LB��B��B�hB�7B�B[#BR�B?}B33B"�B�BB
�;B
��B
�dB
�B
��B
��B
�%B
m�B
cTB
XB
C�B
+B
�B
bB	��B	�mB	�;B	�B	�B	��B	��B	ŢB	�jB	��B	��B	�bB	z�B	k�B	`BB	VB	K�B	>wB	8RB	1'B	+B	!�B	�B	bB	B��B��B��B�B�ZB�B��B��BƨB��B�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�hB�\B�PB�=B�1B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�1B�DB�{B��B��B�B�'B�-B�3B�B��B��B��B��B��B��B��B��B�B�B�-B�-B�'B�-B�FB�qB�^B�jBB�B�)B�ZB�B�fB�B�NB�`B�B��B��B	B	B	B	B	B	PB	�B	�B	�B	�B	�B
[B
sB
+�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B.B.B.B.B.B.B-B-B-B-B-B-B-B-B-B,B)�B(�B �BDB+B	7B�B"�B)�B9XBH�BL�BQ�BT�B[#BaHBcTBiyBp�B}�B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�+Bx�BhsB]/BYBT�B@�B&�B�B��B�NB�
BǮB�LB��B��B�hB�7B�B[#BR�B?}B33B"�B�BB
�;B
��B
�dB
�B
��B
��B
�%B
m�B
cTB
XB
C�B
+B
�B
bB	��B	�mB	�;B	�B	�B	��B	��B	ŢB	�jB	��B	��B	�bB	z�B	k�B	`BB	VB	K�B	>wB	8RB	1'B	+B	!�B	�B	bB	B��B��B��B�B�ZB�B��B��BƨB��B�FB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�hB�\B�PB�=B�1B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�1B�DB�{B��B��B�B�'B�-B�3B�B��B��B��B��B��B��B��B��B�B�B�-B�-B�'B�-B�FB�qB�^B�jBB�B�)B�ZB�B�fB�B�NB�`B�B��B��B	B	B	B	B	B	PB	�B	�B	�B	�B	�B
[B
sB
+�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191710                              AO  ARCAADJP                                                                    20181005191710    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191710  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191710  QCF$                G�O�G�O�G�O�8000            