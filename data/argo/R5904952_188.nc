CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:48Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190548  20181005190548  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�8�1   @��j:��@1�Q���c�C��%1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @9��@�  @�  A   A   AA��Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B_��Bg��Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C�fC  C  C  C  C  C�C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C��C��C�  C��3C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  Dy�D	  D	y�D
  D
� D  D� D  D�fDfD� D  D� D  Dy�D  D�fD  Dy�D  D� D  Dy�D��D� D  Dy�D��Dy�D��Dy�D��D� D��Dy�D  D�fDfD� D��Dy�D��D� D  D� DfD�fD   D y�D ��D!� D"  D"� D#  D#y�D#��D$� D$��D%� D&  D&�fD'  D3  D3� D4  D4y�D4��D5y�D5��D6y�D7  D7y�D7��D8y�D9  D9� D:  D:� D;  D;y�D;��D<� D=fD=�fD>fD>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DC��DD� DEfDE�fDFfDF�fDGfDG� DH  DH�fDIfDI� DI��DJy�DK  DK� DL  DL� DM  DM� DNfDN� DOfDO�fDP  DPy�DP��DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZfDZ� D[  D[�fD\fD\� D]  D]� D^  D^� D_fD_�fD`fD`�fDafDa� Da��Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� DifDiy�Dj  Dj� Dk  Dk� Dl  Dl� Dl��Dmy�Dn  Dn� Do  Do� Dp  Dp� DqfDq�fDrfDr�fDsfDs� DtfDt� Dt��Duy�Du��Dvy�Dw  Dw� Dw�3Dy��D�6�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @L(�@�G�@�G�A��A$��AF=qAf=qA�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�B@BI(�BQ(�BY(�B`BhBq(�By(�B��{B�ǮB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B�ǮB�ǮBȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=Cc�C
J=C0�CJ=CJ=CJ=CJ=CJ=Cc�Cc�CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�1�C�%C�RC�%C�1�C�%C�%C�%C�1�C�1�C�%C�RC�1�C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�RC�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�RC�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�RC�%C�%C�%C�%C�RC�%C�%C�%C�RC�RC�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�1�C�1�C�1�C�%C�RC�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D�)D�D��D�D�)D	�D	�)D
�D
��D�D��D�D��D�D��D�D��D�D�)D�D��D�D�)D�D��D�D�)D)D��D�D�)D)D�)D)D�)D)D��D)D�)D�D��D�D��D)D�)D)D��D�D��D�D��D �D �)D!)D!��D"�D"��D#�D#�)D$)D$��D%)D%��D&�D&��D'�D3�D3��D4�D4�)D5)D5�)D6)D6�)D7�D7�)D8)D8�)D9�D9��D:�D:��D;�D;�)D<)D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC�)DD)DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ)DJ�)DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP�)DQ)DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db)Db��Dc�Dc��Dd�Dd��De)De��Df�Df��Dg�Dg��Dh�Dh��Di�Di�)Dj�Dj��Dk�Dk��Dl�Dl��Dm)Dm�)Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du)Du�)Dv)Dv�)Dw�Dw��Dw��Dy�=D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AɑhAɕ�Aɗ�Aɗ�Aə�Aɛ�Aɟ�Aɡ�Aɡ�Aɥ�Aɣ�Aɛ�Aɛ�Aɝ�Aɣ�A���A�-Aʧ�A��A˟�A���A�`BA�E�A�^5A��yA�  A̼jA̍PA̅A�ffA�(�A�|�AǑhA�ffA�p�A�p�A�(�AƾwAƕ�AƏ\AƋDA�ffA�;dA�/A��A�VA�oAũ�A�A�C�A��#A��A�ƨA�A�hsA�t�A�dZA���A�K�A�oA��A�G�A�bA��DA�C�A��A�5?A��A��\A�+A���A��jA�n�A��A��A�E�A���A��RA�\)A�ffA��hA��wA���A��^A��A�(�A�%A�p�A�?}A��A�K�A�t�A�K�A��9A��9A�z�A���A��A�bA���A�n�A��!A��7A��-A�I�A���A���A�~�A�Q�A��wA�Q�A���A��A��;A�
A~M�A|Q�A{S�Az-Ax��AvVAtn�Ap��Ao�#AmO�Af�uAaA^��AZA�AW�AUC�AR��AP��AOx�AM&�AJE�AG�PADZAA��A@z�A>�A;?}A:E�A9K�A7A6ȴA6A�A5|�A4�jA1�;A0v�A/A,M�A*�A)��A)��A)S�A(�\A'�
A%�hA#�;A#�hA#G�A"ZA!t�A!VA ��A ~�A A�/A�hA1A�PA%A�;AĜA��AM�A�FA�!A��A�RA�PA��A^5A�;AVA�A�TA�7A
��A
1A��A��A�RAt�A�7AC�A��A��A~�A��A ��@�K�@�`B@���@�ȴ@�J@� �@�t�@��\@��@���@��@�C�@�n�@�p�@�V@�Ĝ@�r�@�1@��y@�hs@�7L@���@�@�^5@�7@�%@��@��m@�33@���@��@�A�@㕁@�!@��@�V@���@�I�@�K�@ް!@�@�?}@���@�I�@���@�K�@ڰ!@�{@��@� �@�C�@�$�@��@ԋD@ӕ�@��@�hs@Ь@�z�@��@�1'@Χ�@�5?@Ͳ-@���@�j@�ƨ@˥�@�t�@�l�@�dZ@ʏ\@�5?@���@���@ɩ�@�/@Ȭ@ț�@�(�@�t�@Ə\@�@��@�p�@���@�Ĝ@�(�@�C�@\@���@�X@�V@��u@�I�@��@��P@�o@�ff@�J@�/@���@���@��D@�ƨ@�o@��@��@���@�v�@�x�@�X@�G�@�&�@��/@��9@�A�@�  @��@�=q@�@���@�`B@���@�I�@��@��@���@�\)@�+@�33@�n�@�5?@��#@��@��@��P@�ƨ@�t�@���@�^5@�-@���@��7@�7L@���@�%@��@�z�@�Z@�9X@���@���@�t�@�S�@�33@��@���@�E�@�J@�@���@�p�@�X@�7L@���@��/@��j@���@��D@�A�@��@�t�@�\)@��@���@���@��@��7@�7L@�%@��j@��@�Q�@���@�l�@���@�{@���@��T@��h@���@�j@�b@��;@��@�dZ@�"�@��@��R@�ff@�J@��7@��^@���@���@���@���@�G�@�&�@�V@�(�@�ƨ@�t�@�K�@�o@��@���@�$�@���@��@�`B@�7L@�&�@�/@��u@���@�dZ@�n�@�M�@�ff@���@�O�@��j@�z�@�  @��F@���@�;d@���@�o@�o@���@��@�ȴ@�~�@�@���@��h@���@��j@��j@��9@��@��@��@��@�(�@��;@���@�dZ@�o@��R@�@�O�@��@��9@���@���@�z�@���@z��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AɑhAɕ�Aɗ�Aɗ�Aə�Aɛ�Aɟ�Aɡ�Aɡ�Aɥ�Aɣ�Aɛ�Aɛ�Aɝ�Aɣ�A���A�-Aʧ�A��A˟�A���A�`BA�E�A�^5A��yA�  A̼jA̍PA̅A�ffA�(�A�|�AǑhA�ffA�p�A�p�A�(�AƾwAƕ�AƏ\AƋDA�ffA�;dA�/A��A�VA�oAũ�A�A�C�A��#A��A�ƨA�A�hsA�t�A�dZA���A�K�A�oA��A�G�A�bA��DA�C�A��A�5?A��A��\A�+A���A��jA�n�A��A��A�E�A���A��RA�\)A�ffA��hA��wA���A��^A��A�(�A�%A�p�A�?}A��A�K�A�t�A�K�A��9A��9A�z�A���A��A�bA���A�n�A��!A��7A��-A�I�A���A���A�~�A�Q�A��wA�Q�A���A��A��;A�
A~M�A|Q�A{S�Az-Ax��AvVAtn�Ap��Ao�#AmO�Af�uAaA^��AZA�AW�AUC�AR��AP��AOx�AM&�AJE�AG�PADZAA��A@z�A>�A;?}A:E�A9K�A7A6ȴA6A�A5|�A4�jA1�;A0v�A/A,M�A*�A)��A)��A)S�A(�\A'�
A%�hA#�;A#�hA#G�A"ZA!t�A!VA ��A ~�A A�/A�hA1A�PA%A�;AĜA��AM�A�FA�!A��A�RA�PA��A^5A�;AVA�A�TA�7A
��A
1A��A��A�RAt�A�7AC�A��A��A~�A��A ��@�K�@�`B@���@�ȴ@�J@� �@�t�@��\@��@���@��@�C�@�n�@�p�@�V@�Ĝ@�r�@�1@��y@�hs@�7L@���@�@�^5@�7@�%@��@��m@�33@���@��@�A�@㕁@�!@��@�V@���@�I�@�K�@ް!@�@�?}@���@�I�@���@�K�@ڰ!@�{@��@� �@�C�@�$�@��@ԋD@ӕ�@��@�hs@Ь@�z�@��@�1'@Χ�@�5?@Ͳ-@���@�j@�ƨ@˥�@�t�@�l�@�dZ@ʏ\@�5?@���@���@ɩ�@�/@Ȭ@ț�@�(�@�t�@Ə\@�@��@�p�@���@�Ĝ@�(�@�C�@\@���@�X@�V@��u@�I�@��@��P@�o@�ff@�J@�/@���@���@��D@�ƨ@�o@��@��@���@�v�@�x�@�X@�G�@�&�@��/@��9@�A�@�  @��@�=q@�@���@�`B@���@�I�@��@��@���@�\)@�+@�33@�n�@�5?@��#@��@��@��P@�ƨ@�t�@���@�^5@�-@���@��7@�7L@���@�%@��@�z�@�Z@�9X@���@���@�t�@�S�@�33@��@���@�E�@�J@�@���@�p�@�X@�7L@���@��/@��j@���@��D@�A�@��@�t�@�\)@��@���@���@��@��7@�7L@�%@��j@��@�Q�@���@�l�@���@�{@���@��T@��h@���@�j@�b@��;@��@�dZ@�"�@��@��R@�ff@�J@��7@��^@���@���@���@���@�G�@�&�@�V@�(�@�ƨ@�t�@�K�@�o@��@���@�$�@���@��@�`B@�7L@�&�@�/@��u@���@�dZ@�n�@�M�@�ff@���@�O�@��j@�z�@�  @��F@���@�;d@���@�o@�o@���@��@�ȴ@�~�@�@���@��h@���@��j@��j@��9@��@��@��@��@�(�@��;@���@�dZ@�o@��R@�@�O�@��@��9@���@���@�z�@���@z��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	oB	=qB	�B	�B	�
B	��B
B
1B
�B
�?B
�ZB
�B\B49B
��B
�B
ȴB
�RB
ÖB
��B
�B
�mB	7B.B2-B49B9XBL�BW
BiyBt�Bw�B�uB��B��B�wB��B�;B��BDB�B#�B!�B�B�B!�BVB\BbB\BVBPBDB
=BPBPBJBJBPBPB+BVB5?B=qB1'B/B'�B�BVBB��B�yB�B��B�jB��B��B|�BaHB=qB%B
�fB
�jB
�'B
�!B
�B
��B
�B
S�B
M�B
G�B
?}B
0!B
�B
B	��B	�B	�B	�5B	��B	B	�?B	�!B	��B	�B	��B	��B	�B	y�B	gmB	@�B	#�B	bB	  B�B�B�ZB�ZB�#B�BɺB�wB�RB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�?B�^B�RB�RB�^B�}BĜB��B�dB�^B�qB�}B��B��BŢBƨB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�;B�/B�/B�5B�BB�HB�NB�TB�ZB�ZB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	  B	B	1B	JB	JB	\B	oB	oB	oB	uB	oB	�B	�B	"�B	'�B	%�B	$�B	#�B	%�B	'�B	+B	.B	/B	1'B	2-B	9XB	;dB	=qB	?}B	?}B	A�B	C�B	C�B	F�B	J�B	O�B	T�B	T�B	W
B	YB	YB	[#B	^5B	dZB	hsB	iyB	iyB	jB	jB	l�B	o�B	q�B	t�B	u�B	w�B	x�B	x�B	w�B	z�B	}�B	�B	�B	�B	�1B	�PB	�oB	�uB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��BE�B	�}B	�}B	��B	ĜB	ĜB	ƨB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�NB	�NB	�NB	�TB	�TB	�`B	�mB	�mB	�mB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
VB
VB
bB
oB
oB
uB
oB
oB
hB
bB
bB
hB
oB
uB
uB
{B
�B
'R2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	oB	=qB	�B	�B	�
B	��B
B
1B
�B
�?B
�ZB
�B\B49B
��B
�B
ȴB
�RB
ÖB
��B
�B
�mB	7B.B2-B49B9XBL�BW
BiyBt�Bw�B�uB��B��B�wB��B�;B��BDB�B#�B!�B�B�B!�BVB\BbB\BVBPBDB
=BPBPBJBJBPBPB+BVB5?B=qB1'B/B'�B�BVBB��B�yB�B��B�jB��B��B|�BaHB=qB%B
�fB
�jB
�'B
�!B
�B
��B
�B
S�B
M�B
G�B
?}B
0!B
�B
B	��B	�B	�B	�5B	��B	B	�?B	�!B	��B	�B	��B	��B	�B	y�B	gmB	@�B	#�B	bB	  B�B�B�ZB�ZB�#B�BɺB�wB�RB�'B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�?B�^B�RB�RB�^B�}BĜB��B�dB�^B�qB�}B��B��BŢBƨB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�;B�/B�/B�5B�BB�HB�NB�TB�ZB�ZB�yB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	  B	B	1B	JB	JB	\B	oB	oB	oB	uB	oB	�B	�B	"�B	'�B	%�B	$�B	#�B	%�B	'�B	+B	.B	/B	1'B	2-B	9XB	;dB	=qB	?}B	?}B	A�B	C�B	C�B	F�B	J�B	O�B	T�B	T�B	W
B	YB	YB	[#B	^5B	dZB	hsB	iyB	iyB	jB	jB	l�B	o�B	q�B	t�B	u�B	w�B	x�B	x�B	w�B	z�B	}�B	�B	�B	�B	�1B	�PB	�oB	�uB	�uB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��BE�B	�}B	�}B	��B	ĜB	ĜB	ƨB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�5B	�;B	�BB	�NB	�NB	�NB	�TB	�TB	�`B	�mB	�mB	�mB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B
	7B
	7B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
PB
PB
VB
VB
bB
oB
oB
uB
oB
oB
hB
bB
bB
hB
oB
uB
uB
{B
�B
'R2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.29 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190548                              AO  ARCAADJP                                                                    20181005190548    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190548  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190548  QCF$                G�O�G�O�G�O�8000            