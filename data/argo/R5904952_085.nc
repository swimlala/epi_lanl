CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:24Z creation      
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
_FillValue                  �  =   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  A�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Bh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  E�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  I�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Jx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  N   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  N�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  R�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  V   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  W   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  [�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  _   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    _D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    bD   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    eD   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  hD   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    hp   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ht   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    hx   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    h|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  h�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    h�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    h�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    h�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         h�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         h�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        h�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    h�Argo profile    3.1 1.2 19500101000000  20181005190524  20181005190524  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               UA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��d��s�1   @��e/h^2@1�-V�c]?|�h1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      UA   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B��B  B   B(  B0  B8  B?��BH  BPffBX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C�fC  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C��3C��3C�  C��C��C�  C�  C�  C�  C��3C��3C��3C�  C��C�  C��3C�  C�  C�  C��C�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @J�H@���@ȣ�AQ�A$Q�ADQ�AdQ�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�Bz�B	{B�B{B!{B){B1{B9{B@�BI{BQz�BY{Ba{Bi{Bq{By{B�W
B��=B��=B��=B��=B��=B��=B�W
B��=B��=B��pB��=B��=B��=B��=B��=B��=BĊ=BȊ=B̊=BЊ=BԊ=B؊=B܊=B��=B�=B�W
B�=B��=B�=B��=B��=C ECEC^�CECEC
ECECECECEC+�CECECECECEC +�C"EC$EC&EC(EC*EC,EC.EC0EC2^�C4EC6EC8EC:EC<EC>EC@ECBECDECF^�CHECJ+�CLECNECPECRECTECVECXECZEC\EC^EC`ECbECdECfEChECjEClECnECpECrECtECvECxECzEC|EC~EC�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�"�C�/\C�"�C��C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�"�C�/\C�/\C�"�C�"�C�"�C�"�C�"�C�"�C�"�C��C��C��C��C��C��C�"�C�"�C�/\C�"�C�"�C�"�C�"�C��C�"�C�"�C�"�C�"�C�"�C��C�"�C�/\C�/\C�/\C�/\C�"�C��C��C�"�C�/\C�/\C�"�C�"�C�"�C�"�C��C��C��C�"�C�/\C�"�C��C�"�C�"�C�"�C�/\C�U�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aޥ�Aޥ�Aޣ�Aޥ�Aޥ�Aޣ�Aާ�AެAާ�Aޏ\A�A�Aݺ^A�jA�%A��#A���A܋DA�l�A�VA�XA�;dA�JA��A���A۸RAۉ7A�dZA�
=Aڲ-A�1'A��HAٍPA�-A���Aө�A��AБhAЇ+A���Aѥ�AѺ^AуA�oA��A�bAɩ�Aȉ7A�E�Aś�A�K�A��A��A�n�A��AüjA�A�A�z�A��A��HA�x�A�A�
=A�~�A�S�A�ZA�A��A��PA��^A��/A�A�$�A�-A�bA�r�A�1'A���A�$�A���A��A�x�A�E�A���A���A��A��A���A��A�%A�"�A��-A�ƨA��A��9A�l�A��^A��yA���A���A��A���At�Ax�yAt��Ar �AqO�Ap�HApQ�ApZAp~�Ap�!ApjAjz�Ag��Af�DAd��AahsA_\)A^�jA]VAZ��AYdZAXv�AV�yAT�AT{ASVAQt�AP$�AM�wAL�RAK�7AJA�AG�FAEO�AEC�AD��AA�A?�mA>ȴA:��A8ȴA7K�A61'A5VA1�;A0��A/�-A.��A.��A-�mA*jA(A&�/A%�
A%p�A%XA$��A#p�A"jA!�TA!�hA!K�A ��A�Az�A��A��AbNAC�A��A�A=qA5?A��AhsAE�A��A��Az�A�;At�AĜA�A?}AZA�A�
A�FA`BA�+A�A�HAdZA
ĜA"�A?}A7LAO�A
�HA
n�A	��A	�A�yA	"�A	C�A	�A��Az�A�A�wAp�A�+AS�A33A1'AffAA33A r�A -@��+@�l�@�{@�S�@�/@���@�J@�G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aޥ�Aޥ�Aޣ�Aޥ�Aޥ�Aޣ�Aާ�AެAާ�Aޏ\A�A�Aݺ^A�jA�%A��#A���A܋DA�l�A�VA�XA�;dA�JA��A���A۸RAۉ7A�dZA�
=Aڲ-A�1'A��HAٍPA�-A���Aө�A��AБhAЇ+A���Aѥ�AѺ^AуA�oA��A�bAɩ�Aȉ7A�E�Aś�A�K�A��A��A�n�A��AüjA�A�A�z�A��A��HA�x�A�A�
=A�~�A�S�A�ZA�A��A��PA��^A��/A�A�$�A�-A�bA�r�A�1'A���A�$�A���A��A�x�A�E�A���A���A��A��A���A��A�%A�"�A��-A�ƨA��A��9A�l�A��^A��yA���A���A��A���At�Ax�yAt��Ar �AqO�Ap�HApQ�ApZAp~�Ap�!ApjAjz�Ag��Af�DAd��AahsA_\)A^�jA]VAZ��AYdZAXv�AV�yAT�AT{ASVAQt�AP$�AM�wAL�RAK�7AJA�AG�FAEO�AEC�AD��AA�A?�mA>ȴA:��A8ȴA7K�A61'A5VA1�;A0��A/�-A.��A.��A-�mA*jA(A&�/A%�
A%p�A%XA$��A#p�A"jA!�TA!�hA!K�A ��A�Az�A��A��AbNAC�A��A�A=qA5?A��AhsAE�A��A��Az�A�;At�AĜA�A?}AZA�A�
A�FA`BA�+A�A�HAdZA
ĜA"�A?}A7LAO�A
�HA
n�A	��A	�A�yA	"�A	C�A	�A��Az�A�A�wAp�A�+AS�A33A1'AffAA33A r�A -@��+@�l�@�{@�S�@�/@���@�J@�G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
$�B
<jB
P�B
T�B
`BB
�B
�7B
�VB
�{B
��B
�{B
�uB
�uB
�{B
��B
�{B
�{B
��B
��B
��B
��B
��B
�oB
bB	ƨB	�dB	�fB
7LB
��B
B
ÖB
�XB
��B
6FB
 �B
I�B
��B
�NB+BdZBp�B}�B��B��B�B�B�^BŢBɺB��B��B�wB�B�B  BB�B1'B�B;dB�B�BB�yB�B�B`BBK�B9XBoB
�B
�B
�TB
�B
�3B
�=B
bNB
%�B
PB
B	��B	��B	�B	�B	ɺB	B	�FB	��B	�B	��B	��B	l�B	VB	I�B	F�B	I�B	N�B	W
B	\)B	`BB	e`B	O�B	;dB	/B	!�B	\B	B��B��B�B�B�B�sB�NB�5B�/B�/B�#B�#B�B�#B�B��B��B��B��B�TB�`B�fB�B�B�yB�HB�)B��B��BȴBŢB��BŢB�qB�?B�?B�dB�jB�qBĜB��B��BɺB��B��B�#B�TB�B�B�B��B��B��B�B�B��B	B	B��B�B�NB�BB�/B�5B�;B��B	B��B	B	+B		7B	PB	DB	+B	
=B	JB	VB	�B	/B	>wB	J�B	Q�B	Q�B	N�B	VB	S�B	[#B	cTB	e`B	dZB	dZB	cTB	e`B	ffB	dZB	bNB	e`B	^5B	Q�B	M�B	I�B	F�B	F�B	A�B	7LB	2-B	L�B	G�B	D�B	>wB	@�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
$�B
<jB
P�B
T�B
`BB
�B
�7B
�VB
�{B
��B
�{B
�uB
�uB
�{B
��B
�{B
�{B
��B
��B
��B
��B
��B
�oB
bB	ƨB	�dB	�fB
7LB
��B
B
ÖB
�XB
��B
6FB
 �B
I�B
��B
�NB+BdZBp�B}�B��B��B�B�B�^BŢBɺB��B��B�wB�B�B  BB�B1'B�B;dB�B�BB�yB�B�B`BBK�B9XBoB
�B
�B
�TB
�B
�3B
�=B
bNB
%�B
PB
B	��B	��B	�B	�B	ɺB	B	�FB	��B	�B	��B	��B	l�B	VB	I�B	F�B	I�B	N�B	W
B	\)B	`BB	e`B	O�B	;dB	/B	!�B	\B	B��B��B�B�B�B�sB�NB�5B�/B�/B�#B�#B�B�#B�B��B��B��B��B�TB�`B�fB�B�B�yB�HB�)B��B��BȴBŢB��BŢB�qB�?B�?B�dB�jB�qBĜB��B��BɺB��B��B�#B�TB�B�B�B��B��B��B�B�B��B	B	B��B�B�NB�BB�/B�5B�;B��B	B��B	B	+B		7B	PB	DB	+B	
=B	JB	VB	�B	/B	>wB	J�B	Q�B	Q�B	N�B	VB	S�B	[#B	cTB	e`B	dZB	dZB	cTB	e`B	ffB	dZB	bNB	e`B	^5B	Q�B	M�B	I�B	F�B	F�B	A�B	7LB	2-B	L�B	G�B	D�B	>wB	@�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.27 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190524                              AO  ARCAADJP                                                                    20181005190524    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190524  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190524  QCF$                G�O�G�O�G�O�8000            