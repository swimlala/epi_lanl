CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  =$   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  A�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Fl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  J    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  K   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  N�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  O�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Sh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  W   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  [�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  \�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  `d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    `�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    c�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    f�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  i�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    i�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    i�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    i�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    i�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  i�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    j   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    j    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    j$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         j4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         j8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        j<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    j@Argo profile    3.1 1.2 19500101000000  20181005191736  20181005191736  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$���>1   @��%\�6�@5ٙ�����d�����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�33B�33B�  B���B�  B���B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C;�fC>  C?�fCB  CD  CF  CH  CJ  CL�CN�CP  CR  CT  CU�fCX  CZ  C\�C^�C`�Cb  Cd  Cf  Ch  Ci�fCl  Cn  Co�fCq�fCt  Cv  Cx  Cz  C|�C~  C�fC�  C�  C��C��C��C��C�  C�  C��C��C�  C��3C�  C�  C��3C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C�  C�  C�  C��fC�  C��3C��3C�  C��C��C�  C�  C�  C�  C��C�  C��3C�  C��C��C��C�  C�  C�  C�  C��C��C��C�  C�  C�  C��3C��C��3C��C��C��3C��3C��C��C��3C�  C�  C�  C��C��C��3C��3C�  C��3C��C��C��3C��3C��3C�  C�  C��C�  C��3C�  C�  C��C��C�  C�  C��C��C�  C�  C�  C�  C�  Dy�fD�0 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�AG�A"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�AУ�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBHQ�BP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B��\B�\)B�\)B�\)B�(�B�\)B�\)B�\)B؏\B܏\B�\)B�(�B�\)B�(�B�\)B�\)B�\)B�\)C .C.C.CG�C.C
.C.C.CzC.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8G�C:.C<zC>.C@zCB.CD.CF.CH.CJ.CLG�CNG�CP.CR.CT.CVzCX.CZ.C\G�C^G�C`G�Cb.Cd.Cf.Ch.CjzCl.Cn.CpzCrzCt.Cv.Cx.Cz.C|G�C~.C�
=C�
C�
C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�
C�
=C�
C�
C�
=C�
C�
C�
=C�
=C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
=C�
=C�
=C�
=C�
C�
C�
C��pC�
C�
=C�
=C�
C�#�C�#�C�
C�
C�
C�
C�#�C�
C�
=C�
C�#�C�#�C�#�C�
C�
C�
C�
C�#�C�#�C�#�C�
C�
C�
C�
=C�#�C�
=C�#�C�#�C�
=C�
=C�#�C�#�C�
=C�
C�
C�
C�#�C�#�C�
=C�
=C�
C�
=C�#�C�#�C�
=C�
=C�
=C�
C�
C�#�C�
C�
=C�
C�
C�#�C�#�C�
C�
C�#�C�#�C�
C�
C�
C�
C�
Dy��D�5�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�^5A�`BA�\)A�\)A�^5A�\)A�`BA�`BA�`BA�bNA�bNA�bNA�dZA�ffA�hsA�ffA�ffA�ffA�jA�^5A�M�A�G�A�&�A��A��/AȮAȓuA�r�A�XA�K�A�=qA�5?A�33A�&�A��TAǟ�A�x�A�bNA�bNA�ZA�O�A�=qA�;dA�/A�-A� �A��A�n�A�z�A���A�1'A��/A�9XA�hsA��A�r�A�O�A��uA�"�A���A��\A�dZA��A���A���A�^5A�r�A�oA��wA��A���A�5?A�A�ȴA��A�\)A��yA�VA��A��A��A�  A���A��
A�;dA��PA���A�`BA���A��A�Q�A�\)A���A��FA�oA��#A�%A�?}A�r�A�1'A��9A���A���A��A�`BA�(�A���A���A�ƨA��A��A�;dA��A��/A��A��A��hA��HA��A~�Az�Ay�hAy?}Ax��Ax  Au�Aq�PAm��AlI�Al5?Ak�wAi��Af��Ae�Ac�;Aa�
A`�jA^��A\n�AZȴAY7LAX�AW�AVr�AT��AT9XAS�;AS��ASVAR1'AQt�AO��AOoAN{AMALbAJ��AJjAHVAF^5AC�AAS�A@^5A?t�A=�TA;��A9G�A8�yA85?A7�hA733A6�A6�A6z�A6Q�A5hsA4��A3�A2E�A1��A0v�A/S�A.E�A-O�A,�+A+�A);dA'��A'"�A%33A#�A"�9A!��A!VA��A��Ax�A�uA��A-AQ�A$�A�TA
=A��Az�A �A�A��AbA+A5?A�^A�A��A7LA��A�+A1A��A�A
-A	��A��AM�AȴA��A%An�AK�A1'Ap�A ��A   @�o@��@qj111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�^5A�`BA�\)A�\)A�^5A�\)A�`BA�`BA�`BA�bNA�bNA�bNA�dZA�ffA�hsA�ffA�ffA�ffA�jA�^5A�M�A�G�A�&�A��A��/AȮAȓuA�r�A�XA�K�A�=qA�5?A�33A�&�A��TAǟ�A�x�A�bNA�bNA�ZA�O�A�=qA�;dA�/A�-A� �A��A�n�A�z�A���A�1'A��/A�9XA�hsA��A�r�A�O�A��uA�"�A���A��\A�dZA��A���A���A�^5A�r�A�oA��wA��A���A�5?A�A�ȴA��A�\)A��yA�VA��A��A��A�  A���A��
A�;dA��PA���A�`BA���A��A�Q�A�\)A���A��FA�oA��#A�%A�?}A�r�A�1'A��9A���A���A��A�`BA�(�A���A���A�ƨA��A��A�;dA��A��/A��A��A��hA��HA��A~�Az�Ay�hAy?}Ax��Ax  Au�Aq�PAm��AlI�Al5?Ak�wAi��Af��Ae�Ac�;Aa�
A`�jA^��A\n�AZȴAY7LAX�AW�AVr�AT��AT9XAS�;AS��ASVAR1'AQt�AO��AOoAN{AMALbAJ��AJjAHVAF^5AC�AAS�A@^5A?t�A=�TA;��A9G�A8�yA85?A7�hA733A6�A6�A6z�A6Q�A5hsA4��A3�A2E�A1��A0v�A/S�A.E�A-O�A,�+A+�A);dA'��A'"�A%33A#�A"�9A!��A!VA��A��Ax�A�uA��A-AQ�A$�A�TA
=A��Az�A �A�A��AbA+A5?A�^A�A��A7LA��A�+A1A��A�A
-A	��A��AM�AȴA��A%An�AK�A1'Ap�A ��A   @�o@��@qj111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB8RB9XB;dB<jBA�BD�BF�BI�BK�BL�BM�BN�BN�BO�BVB\)B_;BcTBe`BiyBjBl�Bm�Bo�Bo�Bp�Br�Bt�Bt�B�B�PB�VB�JB�VB�\B�hB��B��B��B��B��B��B��B��B��B��B��B�hB�PB�1B�B� B}�B|�By�Bw�Bq�BcTBS�BF�B<jB;dB:^B7LB0!B�B  B�B�B�HB�B��B�qB�B��B��B�PB�Bz�Bm�BffBffBe`BcTB_;BYBL�B,B�BoBJB1B  B
�B
�`B
��B
�3B
��B
�VB
t�B
YB
P�B
L�B
F�B
=qB
+B
uB
B	��B	��B	��B	�mB	��B	ǮB	�}B	�3B	�B	��B	�bB	�B	{�B	x�B	p�B	iyB	_;B	\)B	ZB	XB	S�B	N�B	I�B	B�B	=qB	7LB	2-B	-B	'�B	"�B	�B	bB	%B��B��B�B�yB�TB�/B�)B�B�B�
B�
B�B��B��B��B��B��B��BǮBƨBB�}B�dB�RB�3B�B�B��B��B��B��B��B��B�hB�VB�=B�1B�B�B~�B}�B|�B{�Bz�By�Bw�Bv�Bt�Br�Bp�Bn�Bm�Bl�BiyBhsBffBe`BdZBffBgmBbNB`BB]/BZBW
BR�BS�BP�BQ�BT�BT�BS�BQ�BP�B	�B
222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB6FB7LB8RB9XB;dB<jBA�BD�BF�BI�BK�BL�BM�BN�BN�BO�BVB\)B_;BcTBe`BiyBjBl�Bm�Bo�Bo�Bp�Br�Bt�Bt�B�B�PB�VB�JB�VB�\B�hB��B��B��B��B��B��B��B��B��B��B��B�hB�PB�1B�B� B}�B|�By�Bw�Bq�BcTBS�BF�B<jB;dB:^B7LB0!B�B  B�B�B�HB�B��B�qB�B��B��B�PB�Bz�Bm�BffBffBe`BcTB_;BYBL�B,B�BoBJB1B  B
�B
�`B
��B
�3B
��B
�VB
t�B
YB
P�B
L�B
F�B
=qB
+B
uB
B	��B	��B	��B	�mB	��B	ǮB	�}B	�3B	�B	��B	�bB	�B	{�B	x�B	p�B	iyB	_;B	\)B	ZB	XB	S�B	N�B	I�B	B�B	=qB	7LB	2-B	-B	'�B	"�B	�B	bB	%B��B��B�B�yB�TB�/B�)B�B�B�
B�
B�B��B��B��B��B��B��BǮBƨBB�}B�dB�RB�3B�B�B��B��B��B��B��B��B�hB�VB�=B�1B�B�B~�B}�B|�B{�Bz�By�Bw�Bv�Bt�Br�Bp�Bn�Bm�Bl�BiyBhsBffBe`BdZBffBgmBbNB`BB]/BZBW
BR�BS�BP�BQ�BT�BT�BS�BQ�BP�B	�B
222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191736                              AO  ARCAADJP                                                                    20181005191736    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191736  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191736  QCF$                G�O�G�O�G�O�8000            