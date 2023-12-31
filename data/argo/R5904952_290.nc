CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:11Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190611  20181005190611  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              "A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��N�n1   @��WO�@1xbM���c���+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     "A   A   A   @�ff@���@���AffA@  A`  A�  A�  A�  A�  A�  A�33A�33A�  B ffB  BffB  B   B'��B0  B8  B@  BG��BO��BX  B`  BhffBp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C�C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~�C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C��C��3C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D��D� D  D� D  D� DfD�fD  D� D  D�fDfD�fD	  D	� D
  D
� D  D� D  D� D  D�fD  D� DfD� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D�fD  D�fD  D� D  D�fD  D� D��D� D   D � D!  D!� D"fD"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'�fD(  D(� D)  D)� D*fD*� D*��D+y�D,  D,� D-  D-y�D.  D.� D/  D/� D0  D0� D1  D1� D2fD2�fD3fD3� D3��D4� D5  D5� D6fD6�fD7  D7� D8  D8� D9  D9�fD:fD:� D;  D;� D<  D<� D=  D=� D>fD>�fD?fD?� D?��D@y�DA  DA� DBfDB�fDC  DCy�DD  DD� DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DJ��DKy�DL  DL�fDM  DMy�DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DSfDS� DT  DT� DUfDU� DV  DV� DW  DW�fDX  DXy�DX��DY� DZfDZ� D[  D[� D\fD\�fD]  D]y�D]��D^� D_  D_� D`  D`y�D`��Day�Db  Db�fDcfDc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Djy�Dj��Dky�Dk��Dly�Dm  Dm� DnfDn�fDofDo� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dw�3Dy�HD�>D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�{A
>A#
=AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�AхA�A�Q�B�\B	(�B�\B(�B!(�B(B1(�B9(�BA(�BHBPBY(�Ba(�Bi�\Bq(�By(�B�ǮB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{B�aHB�aHB��{B�ǮB�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=Cc�Cc�CJ=Cc�CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<c�C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=Cbc�CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=Cxc�CzJ=C|J=C~c�C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�RC�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�RC�RC�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�1�C�1�C�RC�RC�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�RC�%C�%C�1�C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D�)D)D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D)D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&)D&��D'�D'��D(�D(��D)�D)��D*�D*��D+)D+�)D,�D,��D-�D-�)D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4)D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@)D@�)DA�DA��DB�DB��DC�DC�)DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK)DK�)DL�DL��DM�DM�)DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX�)DY)DY��DZ�DZ��D[�D[��D\�D\��D]�D]�)D^)D^��D_�D_��D`�D`�)Da)Da�)Db�Db��Dc�Dc��Dd)Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj�)Dk)Dk�)Dl)Dl�)Dm�Dm��Dn�Dn��Do�Do��Dp)Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy��D�G\D��g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A���A���A���A�A�
=A�bA�bA��TA�|�A�`BA�&�A�1A���A��A��TAʮAʧ�Aʧ�AʬAʣ�Aʝ�AʅA�K�A�ĜA�XA�oAȮAǝ�AƟ�A��Aũ�A�&�A�A��A��A��A��mAľwA�Q�A�bA��mAò-AÃA�"�A¡�A�K�A��A�JA��DA��RA���A��A�&�A���A��/A�33A�=qA�l�A���A�l�A���A�9XA�Q�A��A���A��A��HA�A��A�dZA�VA�5?A���A�dZA��A�JA���A��A�C�A�-A���A�bA���A��HA�  A�\)A��-A�ZA�/A��hA�x�A���A��hA�5?A�;dA�+A���A�ƨA��^A��
A�1A��wA�bNA���A���A|(�Au�PAp�Am��Ag"�Aa&�A[�^AX-AVAQ�hAPJAL�AK�wAI�AE��ADA@�yA?&�A<��A:�!A6$�A5�A4�!A4A�A2��A/ƨA.�jA.�A-%A,n�A+�PA+�A+�A*~�A)�^A)|�A)"�A(ȴA'p�A&ffA%��A$��A$�A#�PA#�A!��A ��A M�AĜA9XA%A1A�;A\)AO�AVA�uA�A��A�+A�;A��A��Ax�A��A�AbNA�A;dA�DA-A�/A�A��AM�A��A��Al�A�7A`BA�A1AdZA�A
��A
�\A
5?A	��A�AZA��Ax�At�A5?A�7AO�A �uA (�@�C�@��+@���@�p�@�\)@�"�@�K�@�bN@�dZ@�-@�A�@�z�@�r�@���@�ƨ@��m@���@�P@��-@��`@�O�@�Ĝ@� �@�1@�Z@�  @�C�@�@�Z@�Z@��@�~�@睲@�;d@�R@�^@�K�@�$�@��@�%@�b@��;@�@�D@�u@��;@��H@�+@��@�@�p�@���@�1'@�1@��@��@��
@�ƨ@�ƨ@ߝ�@�dZ@�@ޗ�@�^5@��@ݺ^@�/@��/@�Ĝ@�Z@ە�@��@���@�~�@�^5@ٲ-@�&�@��`@�1@��
@ם�@׍P@�\)@��@�o@�@֧�@�J@ղ-@�`B@��@���@�Ĝ@ԃ@�bN@�r�@�|�@��@��H@ҏ\@���@�7L@�1'@���@��@�p�@�/@�/@���@�Ĝ@̬@̴9@���@̋D@�Q�@� �@˝�@�"�@�ȴ@ʇ+@�M�@ɩ�@�&�@���@ȋD@ǥ�@Ƨ�@�J@��@�%@ě�@�r�@��m@�+@§�@�@�@+@�^5@��@�`B@��@�1'@�b@���@���@���@��m@���@�33@��!@�^5@��@��@�X@��/@� �@�ƨ@���@��#@�`B@�X@�V@���@��@��F@�\)@�\)@�S�@�\)@�33@�+@�"�@�o@�v�@�@��@���@�?}@��u@��@��@�\)@���@�ff@��@��-@�&�@��`@�Ĝ@���@�r�@��w@�t�@�o@��@��+@�=q@��@�/@��9@��@�(�@���@�l�@�+@�@��!@�ff@�-@�@���@���@�x�@�hs@�`B@�`B@�G�@��/@�  @���@���@�|�@��@�~�@�ff@�=q@�J@��7@��@���@�Z@��@��
@��@�@�ȴ@�=q@�@���@�/@��@��u@�j@�  @��P@��@���@�ff@�5?@�@�X@���@���@�33@��R@�M�@���@�7L@���@���@���@��@�Ĝ@��D@� �@���@�\)@�dZ@���@�+@��R@��+@�ff@�5?@�@��T@�@���@���@��@�`B@�7L@��@�͟@w��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A���A���A���A���A�A�
=A�bA�bA��TA�|�A�`BA�&�A�1A���A��A��TAʮAʧ�Aʧ�AʬAʣ�Aʝ�AʅA�K�A�ĜA�XA�oAȮAǝ�AƟ�A��Aũ�A�&�A�A��A��A��A��mAľwA�Q�A�bA��mAò-AÃA�"�A¡�A�K�A��A�JA��DA��RA���A��A�&�A���A��/A�33A�=qA�l�A���A�l�A���A�9XA�Q�A��A���A��A��HA�A��A�dZA�VA�5?A���A�dZA��A�JA���A��A�C�A�-A���A�bA���A��HA�  A�\)A��-A�ZA�/A��hA�x�A���A��hA�5?A�;dA�+A���A�ƨA��^A��
A�1A��wA�bNA���A���A|(�Au�PAp�Am��Ag"�Aa&�A[�^AX-AVAQ�hAPJAL�AK�wAI�AE��ADA@�yA?&�A<��A:�!A6$�A5�A4�!A4A�A2��A/ƨA.�jA.�A-%A,n�A+�PA+�A+�A*~�A)�^A)|�A)"�A(ȴA'p�A&ffA%��A$��A$�A#�PA#�A!��A ��A M�AĜA9XA%A1A�;A\)AO�AVA�uA�A��A�+A�;A��A��Ax�A��A�AbNA�A;dA�DA-A�/A�A��AM�A��A��Al�A�7A`BA�A1AdZA�A
��A
�\A
5?A	��A�AZA��Ax�At�A5?A�7AO�A �uA (�@�C�@��+@���@�p�@�\)@�"�@�K�@�bN@�dZ@�-@�A�@�z�@�r�@���@�ƨ@��m@���@�P@��-@��`@�O�@�Ĝ@� �@�1@�Z@�  @�C�@�@�Z@�Z@��@�~�@睲@�;d@�R@�^@�K�@�$�@��@�%@�b@��;@�@�D@�u@��;@��H@�+@��@�@�p�@���@�1'@�1@��@��@��
@�ƨ@�ƨ@ߝ�@�dZ@�@ޗ�@�^5@��@ݺ^@�/@��/@�Ĝ@�Z@ە�@��@���@�~�@�^5@ٲ-@�&�@��`@�1@��
@ם�@׍P@�\)@��@�o@�@֧�@�J@ղ-@�`B@��@���@�Ĝ@ԃ@�bN@�r�@�|�@��@��H@ҏ\@���@�7L@�1'@���@��@�p�@�/@�/@���@�Ĝ@̬@̴9@���@̋D@�Q�@� �@˝�@�"�@�ȴ@ʇ+@�M�@ɩ�@�&�@���@ȋD@ǥ�@Ƨ�@�J@��@�%@ě�@�r�@��m@�+@§�@�@�@+@�^5@��@�`B@��@�1'@�b@���@���@���@��m@���@�33@��!@�^5@��@��@�X@��/@� �@�ƨ@���@��#@�`B@�X@�V@���@��@��F@�\)@�\)@�S�@�\)@�33@�+@�"�@�o@�v�@�@��@���@�?}@��u@��@��@�\)@���@�ff@��@��-@�&�@��`@�Ĝ@���@�r�@��w@�t�@�o@��@��+@�=q@��@�/@��9@��@�(�@���@�l�@�+@�@��!@�ff@�-@�@���@���@�x�@�hs@�`B@�`B@�G�@��/@�  @���@���@�|�@��@�~�@�ff@�=q@�J@��7@��@���@�Z@��@��
@��@�@�ȴ@�=q@�@���@�/@��@��u@�j@�  @��P@��@���@�ff@�5?@�@�X@���@���@�33@��R@�M�@���@�7L@���@���@���@��@�Ĝ@��D@� �@���@�\)@�dZ@���@�+@��R@��+@�ff@�5?@�@��T@�@���@���@��@�`B@�7L@��@�͟@w��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	XB	XB	XB	XB	YB	YB	YB	YB	YB	XB	YB	XB	\)B	^5B	`BB	dZB	�'B	�mB	�fB	�ZB	�TB	�NB	�NB	�HB	�5B	�TB	�yB	�B	�B	�B	��B
%B
,B
N�B
cTB
s�B
��B
ȴB
�/B
�BVB�B�B!�B&�B/B;dBJ�BO�BR�BW
B[#BiyB}�B�hB��B�jB�TB�HB�5B��B��B�BB1BhB�B�B�B'�B<jBC�BC�BF�B7LB9XB1'B.B#�B�BuBPB
=BB��B�B�TB�5B��B��B�LB��B��B�hB�1B{�BbNBI�B@�B2-B&�B�B+B
��B
�B
�NB
�/B
�
B
�^B
��B
��B
�=B
[#B
�B	�B	�FB	�oB	{�B	XB	%�B��B�B�BB��BǮB�}B�^B�LB�3B�B��B��B��B��B��B��B��B��B��B�B�3B�?B�?B�FB�qBBÖBBĜBƨB��B��B��BȴBƨBǮB��B��B��B��B��B�B�
B��B��B��B��B��B�
B�B�#B�fB�B�B��B��B��B�B��B	B	+B	  B	1B	JB	JB	
=B	
=B		7B	+B	
=B	�B	#�B	/B	33B	6FB	5?B	5?B	:^B	9XB	8RB	5?B	0!B	!�B	
=B	+B	1B	uB	�B	�B	�B	�B	#�B	&�B	)�B	'�B	&�B	!�B	�B	!�B	/B	M�B	J�B	E�B	M�B	P�B	I�B	H�B	L�B	J�B	B�B	@�B	B�B	I�B	P�B	S�B	T�B	W
B	XB	W
B	S�B	P�B	S�B	[#B	YB	ffB	hsB	e`B	aHB	XB	R�B	P�B	R�B	R�B	S�B	t�B	�B	�+B	�%B	�B	�B	�B	�B	�B	�B	�+B	�DB	�\B	�\B	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�9B	�?B	�FB	�^B	�dB	�dB	�dB	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	B	��B	ÖB	ŢB	��B	��B	ȴB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�)B	�/B	�;B	�BB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B

=B

=B

=B

=B
	7B
1B
1B
+B
%B
B
B
B
B
B
B
B
B
B
  B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
%B
eB
(X222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B	XB	XB	XB	XB	YB	YB	YB	YB	YB	XB	YB	XB	\)B	^5B	`BB	dZB	�'B	�mB	�fB	�ZB	�TB	�NB	�NB	�HB	�5B	�TB	�yB	�B	�B	�B	��B
%B
,B
N�B
cTB
s�B
��B
ȴB
�/B
�BVB�B�B!�B&�B/B;dBJ�BO�BR�BW
B[#BiyB}�B�hB��B�jB�TB�HB�5B��B��B�BB1BhB�B�B�B'�B<jBC�BC�BF�B7LB9XB1'B.B#�B�BuBPB
=BB��B�B�TB�5B��B��B�LB��B��B�hB�1B{�BbNBI�B@�B2-B&�B�B+B
��B
�B
�NB
�/B
�
B
�^B
��B
��B
�=B
[#B
�B	�B	�FB	�oB	{�B	XB	%�B��B�B�BB��BǮB�}B�^B�LB�3B�B��B��B��B��B��B��B��B��B��B�B�3B�?B�?B�FB�qBBÖBBĜBƨB��B��B��BȴBƨBǮB��B��B��B��B��B�B�
B��B��B��B��B��B�
B�B�#B�fB�B�B��B��B��B�B��B	B	+B	  B	1B	JB	JB	
=B	
=B		7B	+B	
=B	�B	#�B	/B	33B	6FB	5?B	5?B	:^B	9XB	8RB	5?B	0!B	!�B	
=B	+B	1B	uB	�B	�B	�B	�B	#�B	&�B	)�B	'�B	&�B	!�B	�B	!�B	/B	M�B	J�B	E�B	M�B	P�B	I�B	H�B	L�B	J�B	B�B	@�B	B�B	I�B	P�B	S�B	T�B	W
B	XB	W
B	S�B	P�B	S�B	[#B	YB	ffB	hsB	e`B	aHB	XB	R�B	P�B	R�B	R�B	S�B	t�B	�B	�+B	�%B	�B	�B	�B	�B	�B	�B	�+B	�DB	�\B	�\B	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�3B	�9B	�?B	�FB	�^B	�dB	�dB	�dB	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	B	��B	ÖB	ŢB	��B	��B	ȴB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�)B	�/B	�/B	�)B	�/B	�;B	�BB	�TB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
1B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B

=B

=B

=B

=B
	7B
1B
1B
+B
%B
B
B
B
B
B
B
B
B
B
  B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
%B
eB
(X222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190611                              AO  ARCAADJP                                                                    20181005190611    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190611  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190611  QCF$                G�O�G�O�G�O�8000            