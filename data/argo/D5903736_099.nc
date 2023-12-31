CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-12-31T13:31:05Z AOML 3.0 creation; 2016-05-31T19:14:41Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141231133105  20160531121441  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               cA   AO  4051_7090_099                   2C  D   APEX                            5368                            041511                          846 @�/���	1   @�/�[��@4��hr��df��n�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    cA   B   B   @���@�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D�3D�C3D��fD��fD� D�FfD�s3D�ɚD�fD�I�D�s3D���D�3D�9�D�p D�ٚD�3D�33D�vfD�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��]@���A z�A z�A@z�Ab{A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�u�B��)B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#�RD$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+RD+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�RDy��D�)D�D)D��\D��\D��D�G\D�t)D�ʐD�\D�J�D�t)D���D�)D�:�D�p�D�ڐD�)D�4)D�w\D�ʐ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�  A���A�  A�  A�A�A�  A�%A�JA�JA�bA�bA�JA�VA�bA�bA�bA�{A��A��A��A�oA�VA�VA�VA�bA�oA�oA�{A�{A�{A�VA�1A�%A�A�A�A�  A��mAŁA�K�A���A��A���A�I�A�
=A�dZA��-A�&�A�~�A��DA��A�1A�bA���A���A��A��uA��FA�n�A��A��A�VA��A��A���A�XA��A���A�XA�  A���A��+A�VA�ȴA��!A���A���A��DA�$�A��A�ƨA���A��DA���A�v�A��^A�=qA�/A��mA��A��
A�C�A��+A�|�A�bA�dZA��A~5?A}O�A{�TA{|�Az��AwC�AtA�Ap�9Ak��Ai�Ah5?Ad�`Ab9XAax�A`�DA]��AZĜAWx�AV��AV��AV{AU�AU7LASG�ARI�APr�AM��AK�AJ1AI��AH^5AG��AG|�AGt�AGXAG+ADȴAB�9AA`BA@1A>�!A<��A:^5A8$�A7hsA6ȴA4�A4A3;dA2n�A1��A0�RA.ZA-|�A,1A*�A)�TA(�A'�mA'%A%�^A${A!�A��An�A{A&�A5?A�A&�A�mA�FA��AS�AoA�A��A�AQ�AO�A�`AE�A�PA�A\)A�HA�jA��AjAbA��A9XAdZA
n�A	C�A�AbAl�A��AG�AA��A �A�;A��AO�A �yA ~�A  �@��@�v�@�-@�@���@��@���@�X@�Z@���@�n�@�&�@���@��@��@��@�\@�?}@��@�+@�G�@�9@�D@�9X@�M�@��m@޸R@���@�x�@�?}@�&�@�7L@�z�@��y@���@�M�@�J@ّh@�&�@�A�@��@���@ա�@Չ7@�Q�@�33@��H@�$�@щ7@�V@�r�@϶F@�$�@�7L@̋D@�t�@��y@�{@���@�`B@�j@��@��@őh@�7L@�bN@þw@�"�@��@�X@�G�@���@���@���@��#@��^@��@��@�  @�K�@��\@�ff@���@�hs@��@�Ĝ@�z�@��
@�\)@���@�$�@��@��h@��@���@��D@�(�@�  @��@���@�K�@���@�ff@�-@��#@�X@���@�r�@��
@���@��@�x�@�/@�r�@�Z@�A�@���@�"�@�@���@�=q@�@���@��@�V@���@�A�@�  @��
@���@�dZ@�
=@���@�E�@��@���@��@�O�@�&�@���@��j@���@�z�@�(�@��F@�K�@�+@���@�ȴ@��!@��+@�{@���@���@�/@��/@��9@��j@��9@��D@�I�@��@���@���@�K�@�o@��H@���@�~�@�M�@�$�@�@�@�x�@�&�@��@�r�@�Z@�A�@�9X@�1@��@�t�@�C�@�+@���@��H@���@��R@�~�@�M�@�@��-@�x�@�X@�?}@�/@�V@��/@�I�@���@��@��m@��w@���@�l�@���@��@��@��H@��@���@���@�v�@�E�@��@��@���@��-@���@�X@�V@��`@�Ĝ@���@��@�z�@�j@��@��F@�K�@�@��@��@���@�n�@�E�@�5?@��@�{@���@��#@�x�@�X@�7L@��@�V@��@��@�  @��F@�dZ@��P@��@�C�@��@�@���@�n�@�$�@���@���@���@�/@��/@���@���@��@�9X@��m@���@�C�@�o@�@��H@��\@�$�@���@���@��h@��@���@��@|�j@t�D@kdZ@a�@V�@P�`@H�9@C"�@=�h@8 �@2��@-�-@(�u@"��@�R@=q@@�7@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A���A�  A���A�  A�  A�A�A�  A�%A�JA�JA�bA�bA�JA�VA�bA�bA�bA�{A��A��A��A�oA�VA�VA�VA�bA�oA�oA�{A�{A�{A�VA�1A�%A�A�A�A�  A��mAŁA�K�A���A��A���A�I�A�
=A�dZA��-A�&�A�~�A��DA��A�1A�bA���A���A��A��uA��FA�n�A��A��A�VA��A��A���A�XA��A���A�XA�  A���A��+A�VA�ȴA��!A���A���A��DA�$�A��A�ƨA���A��DA���A�v�A��^A�=qA�/A��mA��A��
A�C�A��+A�|�A�bA�dZA��A~5?A}O�A{�TA{|�Az��AwC�AtA�Ap�9Ak��Ai�Ah5?Ad�`Ab9XAax�A`�DA]��AZĜAWx�AV��AV��AV{AU�AU7LASG�ARI�APr�AM��AK�AJ1AI��AH^5AG��AG|�AGt�AGXAG+ADȴAB�9AA`BA@1A>�!A<��A:^5A8$�A7hsA6ȴA4�A4A3;dA2n�A1��A0�RA.ZA-|�A,1A*�A)�TA(�A'�mA'%A%�^A${A!�A��An�A{A&�A5?A�A&�A�mA�FA��AS�AoA�A��A�AQ�AO�A�`AE�A�PA�A\)A�HA�jA��AjAbA��A9XAdZA
n�A	C�A�AbAl�A��AG�AA��A �A�;A��AO�A �yA ~�A  �@��@�v�@�-@�@���@��@���@�X@�Z@���@�n�@�&�@���@��@��@��@�\@�?}@��@�+@�G�@�9@�D@�9X@�M�@��m@޸R@���@�x�@�?}@�&�@�7L@�z�@��y@���@�M�@�J@ّh@�&�@�A�@��@���@ա�@Չ7@�Q�@�33@��H@�$�@щ7@�V@�r�@϶F@�$�@�7L@̋D@�t�@��y@�{@���@�`B@�j@��@��@őh@�7L@�bN@þw@�"�@��@�X@�G�@���@���@���@��#@��^@��@��@�  @�K�@��\@�ff@���@�hs@��@�Ĝ@�z�@��
@�\)@���@�$�@��@��h@��@���@��D@�(�@�  @��@���@�K�@���@�ff@�-@��#@�X@���@�r�@��
@���@��@�x�@�/@�r�@�Z@�A�@���@�"�@�@���@�=q@�@���@��@�V@���@�A�@�  @��
@���@�dZ@�
=@���@�E�@��@���@��@�O�@�&�@���@��j@���@�z�@�(�@��F@�K�@�+@���@�ȴ@��!@��+@�{@���@���@�/@��/@��9@��j@��9@��D@�I�@��@���@���@�K�@�o@��H@���@�~�@�M�@�$�@�@�@�x�@�&�@��@�r�@�Z@�A�@�9X@�1@��@�t�@�C�@�+@���@��H@���@��R@�~�@�M�@�@��-@�x�@�X@�?}@�/@�V@��/@�I�@���@��@��m@��w@���@�l�@���@��@��@��H@��@���@���@�v�@�E�@��@��@���@��-@���@�X@�V@��`@�Ĝ@���@��@�z�@�j@��@��F@�K�@�@��@��@���@�n�@�E�@�5?@��@�{@���@��#@�x�@�X@�7L@��@�V@��@��@�  @��F@�dZ@��P@��@�C�@��@�@���@�n�@�$�@���@���@���@�/@��/@���@���@��@�9X@��m@���@�C�@�o@�@��H@��\@�$�@���@���@��hG�O�@���@��@|�j@t�D@kdZ@a�@V�@P�`@H�9@C"�@=�h@8 �@2��@-�-@(�u@"��@�R@=q@@�7@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBgmBffBgmBgmBgmBgmBgmBgmBgmBffBffBffBffBgmBgmBffBgmBgmBffBffBffBffBffBgmBgmBgmBgmBgmBgmBgmBgmBgmBhsBhsBhsBhsBiyBhsBiyBiyBhsBe`B]/BgmBffBdZBaHBXBR�BN�BG�B<jB-B�BPB��B�yB�;B��B��B��BŢBĜB��B��BǮB�^B��B��Bt�B_;BH�B?}B0!B"�B�B�B	7B�B�B��B��B��BȴBŢB�LB��B�BgmBP�BH�BA�B.BDB
�BB
�B
�\B
� B
`BB
B�B
;dB
0!B
,B
#�B
1B	�B	�B	�9B	��B	��B	�DB	�B	~�B	z�B	l�B	`BB	J�B	F�B	C�B	?}B	<jB	8RB	-B	%�B	�B	�B	VB	1B	%B	  B��B��B��B��B��B�B�TB�#B��B��B�}B�3B�-B�!B�B�B�!B�!B�3B�9B�?B�!B�B��B��B��B��B��B��B��B��B��B�uB�hB�bB�VB�JB�DB�7B�1B�1B�+B�+B�%B�B�B�B� B}�B|�Bz�Bx�Bw�Bw�Bv�Bv�Bu�Bt�Br�Br�Bp�Bp�Bo�Bp�Bq�Bs�Bs�Br�Bt�Bw�Bw�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bz�B~�B~�B~�B~�B� B�B�B�B�B�B�B�1B�1B�7B�=B�DB�PB�PB�VB�bB�uB�{B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�-B�'B�'B�B�'B�9B�?B�FB�FB�LB�^B�qB�wB��BB��B�}BÖBǮBɺB��B��B��B��B�
B�B�/B�/B�5B�BB�HB�`B�mB�sB�sB�yB�yB�B�B�B�B�B��B��B��B��B	B	B	B	%B	
=B	JB	bB	�B	�B	�B	�B	�B	!�B	$�B	&�B	&�B	'�B	+B	.B	/B	2-B	6FB	7LB	:^B	:^B	<jB	C�B	G�B	J�B	L�B	O�B	O�B	O�B	S�B	XB	YB	ZB	^5B	`BB	aHB	dZB	ffB	iyB	l�B	n�B	o�B	p�B	q�B	t�B	w�B	y�B	{�B	|�B	� B	�B	�B	�B	�B	�%B	�+B	�7B	�PB	�\B	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�?B	�LB	�RB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	��B	B	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
	7B
	7B

=B
DB

=B

=B

=B
�B
bB
�B
 �B
#�B
(�B
1'B
:^B
?}B
E�B
I�B
M�B
R�B
XB
\)B
`BB
dZB
hsB
l�B
p�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BgnBfgBgnBglBglBgnBgqBgnBgnBfiBfiBfhBfhBgnBgqBfhBgnBgnBfjBfhBfhBfjBfhBgnBgqBgnBgnBgnBgnBgqBgnBgnBhtBhqBhqBhvBi{BhvBi{BizBhsBecB].BgmBfeBd]BaHBXBR�BN�BG�B<iB-B�BQB��B�yB�9B��B��B��BšBěB��B��BǮB�`B��B��Bt�B_;BH�B?|B0B"�B�B�B	4B�B�B��B��B��BȱBšB�JB��B�BglBP�BH�BA�B.BBB
�DB
�	B
�^B
�B
`DB
B�B
;hB
0#B
,B
#�B
4B	�B	�	B	�>B	��B	��B	�MB	�B	B	z�B	l�B	`MB	J�B	F�B	C�B	?�B	<wB	8]B	-B	%�B	�B	�B	cB	@B	1B	 B��B��B��B��B��B�B�cB�0B�B��B��B�EB�>B�4B�B�,B�1B�0B�DB�JB�QB�1B�B�B�B��B��B��B��B��B��B��B��B�zB�uB�gB�\B�WB�JB�CB�CB�>B�=B�8B�1B�+B�B�B~B}Bz�Bx�Bw�Bw�Bv�Bv�Bu�Bt�Br�Br�Bp�Bp�Bo�Bp�Bq�Bs�Bs�Br�Bt�Bw�Bw�Bx�Bx�Bx�Bx�Bx�Bx�Bw�Bz�BBBBB�B�B�"B�"B�*B�(B�*B�AB�BB�IB�NB�XB�_B�bB�gB�sB��B��B��B��B��B��B��B�B�B�B�B�B�B�$B�.B�<B�=B�6B�5B�$B�5B�HB�MB�VB�WB�YB�nB�B��B��BB��B��BäBǺB��B��B��B��B��B�B�)B�=B�=B�@B�OB�UB�nB�xB�B�B�B�B�B�B�B�B�B��B��B��B��B	B	$B	(B	0B	
HB	SB	lB	�B	�B	�B	�B	�B	!�B	$�B	&�B	&�B	'�B	+
B	.B	/#B	24B	6NB	7TB	:jB	:fB	<rB	C�B	G�B	J�B	L�B	O�B	O�B	O�B	TB	XB	Y B	Z%B	^;B	`FB	aNB	daB	flB	i�B	l�B	n�B	o�B	p�B	q�B	t�B	w�B	y�B	{�B	|�B	�B	�B	�B	� B	�&B	�)B	�1B	�<B	�VB	�cB	�hB	�tB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�3B	�AB	�RB	�TB	�dB	�iB	�mB	�vB	�uB	�}B	��B	��B	B	ġB	ĠB	ğB	ğB	ŦB	ƪB	ǴB	ǳB	ȷB	ȸB	��B	ɿB	ɽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�&B	�+B	�9B	�@B	�=B	�GB	�GB	�MB	�PB	�YB	�ZB	�cB	�jB	�jB	�lB	�tB	�vB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
!B
'B
&B
'B
(B
0B
.B
1B
	8B
	:B

AB
EB

@B

AB

AG�O�B
eB
�B
 �B
#�B
(�B
1)B
:^B
?B
E�B
I�B
M�B
R�B
XB
\(B
`CB
dYB
hqB
l�B
p�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.03 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214412016053112144120160531121441  AO  ARCAADJP                                                                    20141231133105    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141231133105  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141231133105  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121441  IP                  G�O�G�O�G�O�                