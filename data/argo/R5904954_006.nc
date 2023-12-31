CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:16:49Z creation      
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
_FillValue                  �  =P   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  B(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  G    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  J�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  K�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  O�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  P�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Xp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Yh   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ]H   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  b    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    bP   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    eP   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    hP   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  kP   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    k|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    k�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    k�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    k�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  k�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    k�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    k�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    k�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         k�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         k�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        k�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    k�Argo profile    3.1 1.2 19500101000000  20181005191649  20181005191649  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @נDV�(N1   @נD��S�@2�n��O��cÍO�;d1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @@  @�33@�  A   A   A@  A^ffA~ffA�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bo��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8  C9�fC<  C>  C@  CB  CC�fCF  CH�CJ�CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCh  Cj  Cl  Cn  Cp  Cq�fCt  Cv�Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C�  C��C��C��C�  C��3C��C��C�  C�  C�  C�  C��C�  C��3C��C�  C��3C��C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C��C��C�  C��C��C�  C�  C��3C��3C��C��C��C��C��C�  C��3C��fC��3C��3C�  C�  C��3C��C�  C��3C�  C��C�  C�  C�  C�  C�  C��3C��C�  C�  C��C��3C��3C�  C��3C��fC��fC��3C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C��C��C�  C��3C�  C��C�  DyvfD�511111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@I��@�  @���AffA"ffABffA`��A�ffA�ffA�33A�33A�33A�33A�33A�33B ��B��B��B��B ��B(��B0��B8��B@��BH��BQ  BX��B`��Bh��Bp34Bx34B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B��B��B�L�B�� B�L�B�L�B�� B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�B�L�C &fC&fC&fC�C&fC
&fC&fC&fC&fC&fC&fC&fC&fC&fC@ C&fC &fC"&fC$&fC&&fC(&fC*&fC,&fC.&fC0&fC2&fC4&fC6@ C8&fC:�C<&fC>&fC@&fCB&fCD�CF&fCH@ CJ@ CL@ CN&fCP&fCR&fCT&fCV&fCX&fCZ&fC\&fC^&fC`&fCb&fCd&fCf�Ch&fCj&fCl&fCn&fCp&fCr�Ct&fCv@ Cx&fCz�C|&fC~&fC�3C�3C�3C�3C�3C�3C�3C�fC�fC�fC�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�fC�3C�  C�3C�fC�3C�3C�3C�  C�  C�  C�3C�fC�  C�  C�3C�3C�3C�3C�  C�3C�fC�  C�3C�fC�  C�3C�3C�3C�3C�3C�fC�3C�  C�  C�  C�,�C�  C�  C�3C�  C�  C�3C�3C�fC�fC�  C�  C�  C�  C�  C�3C�fC���C�fC�fC�3C�3C�fC�  C�3C�fC�3C�  C�3C�3C�3C�3C�3C�fC�  C�3C�3C�  C�fC�fC�3C�fC���C���C�fC�3C�  C�3C�fC�3C�3C�3C�3C�3C�3C�fC�  C�,�C�3C�fC�3C�  C�3Dy� D�9�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA��Aɲ-A�;dA���A�ȴAș�Aǲ-A��AƝ�AƉ7A�dZA�;dA�%A���AœuA�\)A�G�A�?}A�9XA�7LA�33A�&�A��A�{A�A���A��A��A��TA��yA��A��A��yA��HA���AĸRAħ�Aġ�Aĝ�Aĝ�Aģ�Aħ�AĲ-AĸRAİ!Aė�A�r�A�S�A�I�A�M�A�9XA�ĜA�v�A��AA��`A��\A�O�A�33A���A��A�hsA�  A��A�|�A��A�33A�M�A���A�$�A��mA���A��TA��PA�
=A��A��+A���A��uA�hsA�v�A��A�p�A��RA�n�A�ĜA��A��/A�1'A�ffA��uA���A���A�oA�33A��A�oA�v�A���A���A�|�A�A�p�A��!A��7A���A�?}A��A�=qA�Q�A��A�hA~��A{��Ay�Axr�Awp�Avr�AvJAt��As�mAq�Ao�-Am��Ak�
AhffAfv�AcA^�RA]x�A[+AX{AVbNATAS&�ARbNAQ�AKAI�AH$�AF�AFQ�AE�AC;dABJA@-A>n�A<VA:5?A9;dA8��A8�A7A7�A6��A5�A4�9A3��A0�`A-G�A(1A$I�A"�jA!�
A ��A^5A/AƨA 5?A E�AƨAl�AO�AĜA��AVA��A7LA�
AbA�!A  AZA��A��A�+A
�A�DA��A�A�/A1A I�@���@�n�@��-@�&�@��j@��u@��@�r�@�9X@��@�=q@�x�@�u@� �@��
@�t�@�R@웦@�@�v�@�{@��#@�/@�?}@�O�@�@�o@�u@�~�@�M�@�M�@�E�@�n�@�E�@��@�Ĝ@���@��@ܓu@�V@�~�@ԋD@Ӯ@��@�%@�J@͡�@̼j@̋D@̋D@̋D@�z�@}L�@j6�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ffA��Aɲ-A�;dA���A�ȴAș�Aǲ-A��AƝ�AƉ7A�dZA�;dA�%A���AœuA�\)A�G�A�?}A�9XA�7LA�33A�&�A��A�{A�A���A��A��A��TA��yA��A��A��yA��HA���AĸRAħ�Aġ�Aĝ�Aĝ�Aģ�Aħ�AĲ-AĸRAİ!Aė�A�r�A�S�A�I�A�M�A�9XA�ĜA�v�A��AA��`A��\A�O�A�33A���A��A�hsA�  A��A�|�A��A�33A�M�A���A�$�A��mA���A��TA��PA�
=A��A��+A���A��uA�hsA�v�A��A�p�A��RA�n�A�ĜA��A��/A�1'A�ffA��uA���A���A�oA�33A��A�oA�v�A���A���A�|�A�A�p�A��!A��7A���A�?}A��A�=qA�Q�A��A�hA~��A{��Ay�Axr�Awp�Avr�AvJAt��As�mAq�Ao�-Am��Ak�
AhffAfv�AcA^�RA]x�A[+AX{AVbNATAS&�ARbNAQ�AKAI�AH$�AF�AFQ�AE�AC;dABJA@-A>n�A<VA:5?A9;dA8��A8�A7A7�A6��A5�A4�9A3��A0�`A-G�A(1A$I�A"�jA!�
A ��A^5A/AƨA 5?A E�AƨAl�AO�AĜA��AVA��A7LA�
AbA�!A  AZA��A��A�+A
�A�DA��A�A�/A1A I�@���@�n�@��-@�&�@��j@��u@��@�r�@�9X@��@�=q@�x�@�u@� �@��
@�t�@�R@웦@�@�v�@�{@��#@�/@�?}@�O�@�@�o@�u@�~�@�M�@�M�@�E�@�n�@�E�@��@�Ĝ@���@��@ܓu@�V@�~�@ԋD@Ӯ@��@�%@�J@͡�@̼j@̋D@̋D@̋D@�z�@}L�@j6�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��BBBBBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  BBBBB  B  B
��B
��BBBB1B
=BJB\BVBJB1B	7BJBDBPB�B"�B-BB�Bm�B�1B�B��B�B�B�BE�BH�BO�BR�BS�BP�BQ�BO�BYB�B�JBz�Bl�BZB<jB#�B	7B��B�B�B�5B�B+BB��B�B�fB�B��B�}B��B�JBp�BO�B>wB49B'�BbB
�B
�B
��B
��B
�-B
y�B
Q�B
K�B
@�B
1'B
�B
VB
1B	��B	�ZB	�5B	�#B	�B	�TB	�BB	�B	ÖB	�B	��B	� B	m�B	cTB	K�B	/B	#�B	�B	+B	B��B�B�yB�;BɺB��B�jB�LB�9B�B��B��B��B��B�B�FB�}B��BƨBɺB��B��B�
B�#B�)B��B�wB��B�+Bz�Bv�Br�By�B��B��B�'B�XBĜBB��BǮB��BɺBÖB�qB�?B�B��B��B�hB�B~�B� Bu�Bs�Bu�Br�Bl�B`BBXBP�BR�BR�BT�BVBXB[#B_;BaHBgmBjBgmBdZBbNBbNBaHB`BBcTBdZBdZBe`Be`BffBk�Bq�Bs�Bm�BiyBiyBiyBiyBm�Bz�B�B�DB�JB�JB�JB�hB��B��B��B�B�!B�9B�RB�RB�LB�RB�RB�RB�RB
�B
*�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222B
��B
��BBBBBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  BBBBB  B  B
��B
��BBBB1B
=BJB\BVBJB1B	7BJBDBPB�B"�B-BB�Bm�B�1B�B��B�B�B�BE�BH�BO�BR�BS�BP�BQ�BO�BYB�B�JBz�Bl�BZB<jB#�B	7B��B�B�B�5B�B+BB��B�B�fB�B��B�}B��B�JBp�BO�B>wB49B'�BbB
�B
�B
��B
��B
�-B
y�B
Q�B
K�B
@�B
1'B
�B
VB
1B	��B	�ZB	�5B	�#B	�B	�TB	�BB	�B	ÖB	�B	��B	� B	m�B	cTB	K�B	/B	#�B	�B	+B	B��B�B�yB�;BɺB��B�jB�LB�9B�B��B��B��B��B�B�FB�}B��BƨBɺB��B��B�
B�#B�)B��B�wB��B�+Bz�Bv�Br�By�B��B��B�'B�XBĜBB��BǮB��BɺBÖB�qB�?B�B��B��B�hB�B~�B� Bu�Bs�Bu�Br�Bl�B`BBXBP�BR�BR�BT�BVBXB[#B_;BaHBgmBjBgmBdZBbNBbNBaHB`BBcTBdZBdZBe`Be`BffBk�Bq�Bs�Bm�BiyBiyBiyBiyBm�Bz�B�B�DB�JB�JB�JB�hB��B��B��B�B�!B�9B�RB�RB�LB�RB�RB�RB�RB
�B
*�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.15 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191649                              AO  ARCAADJP                                                                    20181005191649    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191649  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191649  QCF$                G�O�G�O�G�O�8000            