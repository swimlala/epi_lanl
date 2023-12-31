CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:23Z creation      
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
_FillValue                  �  =L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  >D   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  B    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  F�   TEMP_QC          
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
resolution        :�o     �  Tx   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  XT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  YL   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  ](   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  a�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    b,   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    e,   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    h,   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  k,   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    kX   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    k\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    k`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    kd   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  kh   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    k�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    k�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    k�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         k�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         k�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        k�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    k�Argo profile    3.1 1.2 19500101000000  20181005190523  20181005190523  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               TA   AO  6431                            2B  A   APEX                            7466                            062512                          846 @�Ƥ�J��1   @�ƥ���@1�$�/�cb�1'1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      TA   A   A   @�ff@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8ffB@  BG��BO��BX  B`ffBhffBpffBx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��3C�  C��C�  C�  C��C��C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C��3C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C��3C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C�  C��C��C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�ٚ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�
=@���A Q�A Q�A@Q�A^�RA�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B�B{B{B {B({B0{B8z�B@{BG�BO�BX{B`z�Bhz�Bpz�Bx{B�
=B�
=B�
=B�=pB�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�=pB�
=B�
=B�=pB�=pB�
=B�=pB�
=B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCCCCC C"C#�C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCQ�CTCVCXCZC\C^C`Cb�CdCfChCjClCnCo�CrCtCvCxCzC|C~C��C��C��C��C��C��C��C�\C�\C��C���C��C��C���C��C��C��C��C��C��C��C��C�\C�\C��C��C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C��C��C�\C��C��C�\C�\C���C��C�\C��C��C�\C�\C��C��C��C��C�\C�\C��C���C��C��C��C��C���C���C���C��C��C���C��C��C��C��C��C�\C��C��C��C��C���C���C��C��C��C���C���C���C��C��C��C�\C��C���C��C��C��C���C��C�\C�\C��C���C��C��C��C��C��C�\C��C��C��C��C��C��C��C��C�\C��)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aݴ9Aݰ!Aݲ-AݶFAݺ^A�ĜA�ĜA���A���A���A���A���A���A���A���A���A���Aݡ�A�C�AۼjA۬Aۛ�A�jA�E�A�9XA�/A�+A�&�A��A�VA��`Aڲ-Aک�Aڗ�AڋDA�Q�A���AّhA��A�E�A��A�  A�A�~�A̸RAʉ7A��/A�/A���Aş�A���A��A���AÓuA���A�I�A��
A�/A��A��;A�oA���A���A��A��#A�hsA�A���A��9A�1'A���A�I�A�"�A��A�A�A�VA��HA�
=A�/A���A���A��wA�JA��HA��A�dZA�v�A�1'A�-A�`BA��A��;A�r�A��PA�A�1A�I�A��A��hA�O�A���A|�9AxAvVAr�An��Al1Aj�Ah��Af��Ae|�Ac|�A_%A\VA[�AY��AW|�AT��AS�AQ%AN�RAK�;AF��AE33ACt�AA�^A@-A?;dA=33A;"�A8n�A7�A69XA2�DA0~�A0�A.�A-�TA,jA+�wA++A*ffA(�HA'��A'`BA'+A&�yA&VA%�A$�A$  A"n�A!\)A ĜA��A5?A�A/An�A�wAK�A%A~�A�TA�yA�AffA�A��A�A�yA�-A��A��AXA�+A\)A1A
��A
��A	G�A  At�AA	�-A
E�A
 �A	A	A	��A	�TA	�A	�A	�TA	�;A
  A
JA	�A	hsA	G�A	+A	A�jA��A+A�A��A1'AO�A�\A�
A�`AbA ĜA Q�A �@��h@�ff@���@�{@�%@�=q@�`B@�Ĝ@�  @���@�Q�@��`@�@��m@�G�@ꟾ@�5?@�/@�(�@�ƨ@�;d@�K�@��@��#@�1@�=q@�%@�bN@�C�@�-@�n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aݴ9Aݰ!Aݲ-AݶFAݺ^A�ĜA�ĜA���A���A���A���A���A���A���A���A���A���Aݡ�A�C�AۼjA۬Aۛ�A�jA�E�A�9XA�/A�+A�&�A��A�VA��`Aڲ-Aک�Aڗ�AڋDA�Q�A���AّhA��A�E�A��A�  A�A�~�A̸RAʉ7A��/A�/A���Aş�A���A��A���AÓuA���A�I�A��
A�/A��A��;A�oA���A���A��A��#A�hsA�A���A��9A�1'A���A�I�A�"�A��A�A�A�VA��HA�
=A�/A���A���A��wA�JA��HA��A�dZA�v�A�1'A�-A�`BA��A��;A�r�A��PA�A�1A�I�A��A��hA�O�A���A|�9AxAvVAr�An��Al1Aj�Ah��Af��Ae|�Ac|�A_%A\VA[�AY��AW|�AT��AS�AQ%AN�RAK�;AF��AE33ACt�AA�^A@-A?;dA=33A;"�A8n�A7�A69XA2�DA0~�A0�A.�A-�TA,jA+�wA++A*ffA(�HA'��A'`BA'+A&�yA&VA%�A$�A$  A"n�A!\)A ĜA��A5?A�A/An�A�wAK�A%A~�A�TA�yA�AffA�A��A�A�yA�-A��A��AXA�+A\)A1A
��A
��A	G�A  At�AA	�-A
E�A
 �A	A	A	��A	�TA	�A	�A	�TA	�;A
  A
JA	�A	hsA	G�A	+A	A�jA��A+A�A��A1'AO�A�\A�
A�`AbA ĜA Q�A �@��h@�ff@���@�{@�%@�=q@�`B@�Ĝ@�  @���@�Q�@��`@�@��m@�G�@ꟾ@�5?@�/@�(�@�ƨ@�;d@�K�@��@��#@�1@�=q@�%@�bN@�C�@�-@�n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
DB
JB
JB
PB
�B
p�B
�B
�B
�B
�DB
�VB
�VB
�\B
�\B
�bB
�hB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
�'B
�'B
�9B
�/B�B"�B\B
�B
�BJBoB=qBjB�B��B�B�dB�RB��B�#B�HB�BB�B�B+B;dB9XB<jBB�BVBv�B}�B�B� B�%B�B~�Be`BaHB]/BP�BA�B�BB��B�B�B��B�LB��B�DBcTBG�B2-B�B
��B
�/B
�LB
�B
C�B
�B	�fB	�B	�oB	�B	cTB	I�B	8RB	/B	'�B	�B	uB	%B��B��B��B�B�sB�TB�;B�)B�B�B��BȴB��B�5B�ZB�NB�
B��B�
B�BɺB�?B�-B�}B�^B�3B�-B�B�B�B�'B�?B�?B�FB�XB�dB�wB�}BĜB��B�B�;B�`B�B�B�B��B��B��B��B��B��B��B�B�yB�TB�;B�B�
B�#B�)B�BB�`B�mB�mB�TB�HB�ZB�`B�`B�B	B	&�B	33B	:^B	C�B	I�B	M�B	P�B	S�B	R�B	R�B	T�B	ZB	^5B	^5B	\)B	^5B	^5B	bNB	aHB	aHB	e`B	ffB	jB	k�B	hsB	dZB	_;B	\)B	\)B	YB	VB	S�B	K�B	A�B	;dB	33B	1'B	-B	,B	-B	,B	,B	49B	<jB	;dB	7LB	1'B	,B	.B	/B	/B	0!B	2-B	=qB	H�B	J�B	G�B	A�B	?}B	>wB	=qB	@�B	M�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
DB
JB
JB
PB
�B
p�B
�B
�B
�B
�DB
�VB
�VB
�\B
�\B
�bB
�hB
�uB
�{B
��B
��B
��B
��B
��B
��B
��B
�'B
�'B
�9B
�/B�B"�B\B
�B
�BJBoB=qBjB�B��B�B�dB�RB��B�#B�HB�BB�B�B+B;dB9XB<jBB�BVBv�B}�B�B� B�%B�B~�Be`BaHB]/BP�BA�B�BB��B�B�B��B�LB��B�DBcTBG�B2-B�B
��B
�/B
�LB
�B
C�B
�B	�fB	�B	�oB	�B	cTB	I�B	8RB	/B	'�B	�B	uB	%B��B��B��B�B�sB�TB�;B�)B�B�B��BȴB��B�5B�ZB�NB�
B��B�
B�BɺB�?B�-B�}B�^B�3B�-B�B�B�B�'B�?B�?B�FB�XB�dB�wB�}BĜB��B�B�;B�`B�B�B�B��B��B��B��B��B��B��B�B�yB�TB�;B�B�
B�#B�)B�BB�`B�mB�mB�TB�HB�ZB�`B�`B�B	B	&�B	33B	:^B	C�B	I�B	M�B	P�B	S�B	R�B	R�B	T�B	ZB	^5B	^5B	\)B	^5B	^5B	bNB	aHB	aHB	e`B	ffB	jB	k�B	hsB	dZB	_;B	\)B	\)B	YB	VB	S�B	K�B	A�B	;dB	33B	1'B	-B	,B	-B	,B	,B	49B	<jB	;dB	7LB	1'B	,B	.B	/B	/B	0!B	2-B	=qB	H�B	J�B	G�B	A�B	?}B	>wB	=qB	@�B	M�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.02 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190523                              AO  ARCAADJP                                                                    20181005190523    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190523  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190523  QCF$                G�O�G�O�G�O�18000           