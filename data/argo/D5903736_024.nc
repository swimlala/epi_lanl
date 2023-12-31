CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:18Z AOML 3.0 creation; 2016-05-31T19:14:28Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230518  20160531121428  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_024                   2C  D   APEX                            5368                            041511                          846 @�n��5��1   @�n�r��
@3��E���d�1&�x�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
fD
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dys3D�3D�9�D�p D��3D�fD�9�D�vfD�� D��D�C3D���D��3D�fD�)�D�|�D��D���D�Y�D��D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @A�@���@���A z�A z�A@z�A`z�A�=qA�=qA�=qA�=qA�
>A�
>A�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C)�C+�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	�RD
RD
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dk��Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtn�DyuD�)D�:�D�p�D��)D�\D�:�D�w\D���D��D�D)D���D��)D�\D�*�D�}�D��D���D�Z�D��D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�hsA�ffA�dZA�bNA�dZA�bNA�bNA�dZA�dZA�bNA�ZA�VA�O�A�I�A�I�A�G�A�A�A�;dA�7LA�/A�&�A�$�A�$�A�"�A��A��A�{A�oA�oA�oA�JA�1AϋDA���A�33A�I�A�"�A���A�G�A�A�A���A�A�A�A�A��hA��A�z�A���A�|�A�A�=qA��PA���A��A�oA��#A�ƨA�%A�JA�p�A���A�VA��;A��/A���A�l�A��!A�K�A��A�bA�&�A�1A���A���A���A��A�hsA�7LA�-A�1'A��RA���A���A�M�A��;A�33A��#A�$�A��yA�dZA�r�A��A�t�A���A��A�$�A�?}A���A���A��;A�|�A���A�t�A�\)A�/A�x�A���A��A��A�oA���A�^5A���A�XA33A|(�AxbAt^5Ap�Am�AmoAk��Aix�Af�yAd�Ad�Ac33Aal�A_�TA_7LA]�7A\ZA[oAY��AX1'AW`BATI�ARr�AQp�AO��AM�AL(�AK%AI?}AF��AE��ACAA+A@�A?�A=��A<�HA;��A9|�A7�
A6(�A4^5A2�RA0�/A0n�A01A-hsA+\)A+�A+oA*�A*r�A)+A&-A$~�A#`BA ĜA�7AjA;dA��A7LA�A9XA�A�/A=qA �At�AA�9AM�A��AC�A�9A�A
5?A	A5?A{A�TA\)A�+A(�A��A�Al�A��A=qA�A��AdZA��A��A��AXA ��@�S�@�n�@���@��D@�ȴ@�bN@��@���@�J@�%@��@�`B@�u@�@�+@�@���@��@�p�@�"�@��@��@�"�@��T@�u@�  @㝲@���@���@���@��m@���@�K�@��@���@�hs@���@�^5@ܴ9@�+@�@�O�@���@֗�@�&�@�1'@Ӯ@ҏ\@мj@θR@�v�@�V@�=q@��@���@��@��@ˍP@���@Ɂ@Ǯ@�l�@��@���@�r�@�ȴ@���@�O�@��@�Z@��@��R@�/@���@�A�@��@�J@���@��D@�  @�\)@�$�@��h@�p�@���@���@�|�@��!@��#@���@��j@�(�@��F@�t�@�C�@�@��+@�M�@�J@���@�O�@��@��`@�Ĝ@��@��
@�S�@�"�@��H@�ȴ@���@��+@�n�@�^5@���@��7@��@��u@�(�@�ƨ@���@�t�@��@�ff@�$�@��^@�hs@�X@�?}@��@���@��@�Ĝ@�bN@� �@��@��;@��
@��w@���@�dZ@�S�@�C�@�o@�
=@���@�n�@�J@�@��@�p�@�?}@���@��@�A�@��@��@�dZ@�"�@��@��R@��+@�E�@�J@�@���@�x�@�X@��@��j@�z�@�(�@��@���@��P@��@�|�@�S�@��y@���@�~�@�n�@�-@�{@���@�@��^@�x�@�V@���@��j@��D@��@�A�@���@��
@�ƨ@�|�@�C�@���@�v�@�M�@��@��^@��@���@�Ĝ@��D@�A�@�1@��@��w@�t�@��@��@�v�@�{@��7@�X@�X@�/@�Ĝ@���@��u@��D@�Z@�(�@�  @�dZ@�"�@��!@��\@�n�@��@��#@��^@��-@��-@��-@��-@��-@���@�hs@�O�@�7L@�%@��@�Z@� �@��m@��P@�\)@�;d@�@���@��R@��\@�5?@�{@��@��-@���@�G�@�V@��`@�Ĝ@���@��u@�Z@�1@��F@�|�@�dZ@�;d@��@���@���@�5?@��@|j@qx�@j��@b�@X  @R^5@H�`@B^5@<1@6$�@/�P@)�7@$�@ ��@��@r�@(�@ȴ@	�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�hsA�ffA�dZA�bNA�dZA�bNA�bNA�dZA�dZA�bNA�ZA�VA�O�A�I�A�I�A�G�A�A�A�;dA�7LA�/A�&�A�$�A�$�A�"�A��A��A�{A�oA�oA�oA�JA�1AϋDA���A�33A�I�A�"�A���A�G�A�A�A���A�A�A�A�A��hA��A�z�A���A�|�A�A�=qA��PA���A��A�oA��#A�ƨA�%A�JA�p�A���A�VA��;A��/A���A�l�A��!A�K�A��A�bA�&�A�1A���A���A���A��A�hsA�7LA�-A�1'A��RA���A���A�M�A��;A�33A��#A�$�A��yA�dZA�r�A��A�t�A���A��A�$�A�?}A���A���A��;A�|�A���A�t�A�\)A�/A�x�A���A��A��A�oA���A�^5A���A�XA33A|(�AxbAt^5Ap�Am�AmoAk��Aix�Af�yAd�Ad�Ac33Aal�A_�TA_7LA]�7A\ZA[oAY��AX1'AW`BATI�ARr�AQp�AO��AM�AL(�AK%AI?}AF��AE��ACAA+A@�A?�A=��A<�HA;��A9|�A7�
A6(�A4^5A2�RA0�/A0n�A01A-hsA+\)A+�A+oA*�A*r�A)+A&-A$~�A#`BA ĜA�7AjA;dA��A7LA�A9XA�A�/A=qA �At�AA�9AM�A��AC�A�9A�A
5?A	A5?A{A�TA\)A�+A(�A��A�Al�A��A=qA�A��AdZA��A��A��AXA ��@�S�@�n�@���@��D@�ȴ@�bN@��@���@�J@�%@��@�`B@�u@�@�+@�@���@��@�p�@�"�@��@��@�"�@��T@�u@�  @㝲@���@���@���@��m@���@�K�@��@���@�hs@���@�^5@ܴ9@�+@�@�O�@���@֗�@�&�@�1'@Ӯ@ҏ\@мj@θR@�v�@�V@�=q@��@���@��@��@ˍP@���@Ɂ@Ǯ@�l�@��@���@�r�@�ȴ@���@�O�@��@�Z@��@��R@�/@���@�A�@��@�J@���@��D@�  @�\)@�$�@��h@�p�@���@���@�|�@��!@��#@���@��j@�(�@��F@�t�@�C�@�@��+@�M�@�J@���@�O�@��@��`@�Ĝ@��@��
@�S�@�"�@��H@�ȴ@���@��+@�n�@�^5@���@��7@��@��u@�(�@�ƨ@���@�t�@��@�ff@�$�@��^@�hs@�X@�?}@��@���@��@�Ĝ@�bN@� �@��@��;@��
@��w@���@�dZ@�S�@�C�@�o@�
=@���@�n�@�J@�@��@�p�@�?}@���@��@�A�@��@��@�dZ@�"�@��@��R@��+@�E�@�J@�@���@�x�@�X@��@��j@�z�@�(�@��@���@��P@��@�|�@�S�@��y@���@�~�@�n�@�-@�{@���@�@��^@�x�@�V@���@��j@��D@��@�A�@���@��
@�ƨ@�|�@�C�@���@�v�@�M�@��@��^@��@���@�Ĝ@��D@�A�@�1@��@��w@�t�@��@��@�v�@�{@��7@�X@�X@�/@�Ĝ@���@��u@��D@�Z@�(�@�  @�dZ@�"�@��!@��\@�n�@��@��#@��^@��-@��-@��-@��-@��-@���@�hs@�O�@�7L@�%@��@�Z@� �@��m@��P@�\)@�;d@�@���@��R@��\@�5?@�{@��@��-@���@�G�@�V@��`@�Ĝ@���@��u@�Z@�1@��F@�|�@�dZ@�;d@��@���@���@�5?@��@|j@qx�@j��@b�@X  @R^5@H�`@B^5@<1@6$�@/�P@)�7@$�@ ��@��@r�@(�@ȴ@	�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBz�B{�B{�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�B{�B{�B{�B|�B|�B|�B|�B{�B{�Bz�Bz�Bz�Bz�Bz�Bx�BiyBYBJ�B9XB.B%�B �B�B�B�B�BbBVBJB	7B	7B	7B%BBB  B  BB��B��B��B��B�B��B��B��B��B��B��B�B�;B��BɺBǮBĜB�^B�B��B��B�JB|�BgmBaHBW
BN�BO�BT�BL�B1'B#�B"�B�BPBB��B�B�XB��B��B�1BdZB2-B�B
��B
�B
ŢB
�9B
��B
�{B
�bB
�1B
�B
� B
{�B
s�B
cTB
W
B
@�B
�B	��B	�B	ĜB	�qB	�9B	��B	��B	�PB	�+B	�B	v�B	jB	bNB	YB	Q�B	J�B	D�B	>wB	9XB	-B	%�B	�B	�B	bB	1B	B��B��B�B�TB�HB�NB�BB�TB�NB�5B��B��B��B��B��B��BȴBŢB�wB�LB�LB�LB�FB�?B�!B��B��B��B��B��B��B��B��B�oB�oB�DB�+B�B� B~�Bz�Bu�Bt�Bt�Bs�Bp�Bn�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bn�Bm�Bn�Bn�Bm�Bm�Bm�Bl�Bk�Bl�Bm�Bl�Bl�Bk�BjBm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bo�Bp�Br�Br�Br�Br�Br�Br�Bq�Bp�Bo�Bl�BjBhsBgmBhsBiyBm�Bq�Br�Bs�Bt�Bt�Bt�Bw�Bw�Bw�Bx�B~�B�B�B�B�B�7B�JB�VB�\B�\B�bB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�LB�jB��B��BĜBĜBŢB��B��B��B��B�B�/B�;B�TB�mB�B�B�B�B��B��B	  B	B	
=B	JB	\B	oB	{B	�B	�B	�B	�B	�B	!�B	#�B	$�B	&�B	&�B	(�B	-B	1'B	2-B	49B	49B	5?B	6FB	7LB	7LB	9XB	<jB	A�B	C�B	F�B	H�B	I�B	J�B	N�B	Q�B	S�B	W
B	YB	ZB	[#B	\)B	]/B	]/B	^5B	aHB	cTB	dZB	dZB	e`B	e`B	ffB	hsB	iyB	iyB	jB	jB	l�B	o�B	r�B	t�B	v�B	v�B	x�B	z�B	|�B	�B	�B	�B	�%B	�1B	�7B	�DB	�JB	�VB	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�?B	�FB	�LB	�RB	�^B	�qB	�}B	�}B	�}B	��B	B	ÖB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�#B	�5B	�;B	�BB	�NB	�ZB	�fB	�fB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B
	7B
DB
PB
�B
�B
&�B
+B
33B
;dB
?}B
F�B
L�B
Q�B
XB
^5B
bNB
ffB
jB
m�B
q�B
v�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bz�B{�B{�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�Bz�B{�B{�B{�B|�B|�B|�B|�B{�B{�Bz�Bz�Bz�Bz�Bz�Bx�Bi{BYBJ�B9YB.B%�B �B�B�B�B�BaBWBJB	4B	5B	5B%BBB  B��BB��B��B��B��B�B��B��B��B��B��B��B�B�;B��BɶBǮBĚB�\B�B��B��B�KB|�BgkBaBBWBN�BO�BT�BL�B1%B#�B"�B�BNBB��B� B�RB��B��B�0BdVB2,B�B
��B
�B
şB
�;B
��B
�|B
�cB
�/B
�B
�B
{�B
s�B
cWB
WB
@�B
�B	��B	�B	ĤB	�wB	�@B	��B	��B	�[B	�3B	�B	v�B	j�B	bXB	Y#B	Q�B	J�B	D�B	>�B	9dB	-B	%�B	�B	�B	pB	AB	B��B��B��B�cB�VB�^B�QB�dB�[B�DB�B��B��B��B��B��B��BűB��B�_B�\B�[B�VB�OB�1B��B��B��B��B��B��B��B��B�B�B�XB�<B�%B�BBz�Bu�Bt�Bt�Bs�Bp�Bn�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bn�Bm�Bn�Bn�Bm�Bm�Bm�Bl�Bk�Bl�Bm�Bl�Bl�Bk�Bj�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bn�Bo�Bp�Br�Br�Br�Br�Br�Br�Bq�Bp�Bo�Bl�Bj�Bh�Bg�Bh�Bi�Bm�Bq�Br�Bs�Bt�Bt�Bt�Bw�Bw�Bw�Bx�BB� B�+B�,B�0B�HB�YB�hB�lB�kB�rB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�4B�\B�vB��B��BĬBīBŰB��B��B��B��B�B�;B�GB�bB�zB�B��B�B��B��B��B	 
B	"B	
IB	UB	eB	{B	�B	�B	�B	�B	�B	�B	!�B	#�B	$�B	&�B	&�B	(�B	-B	10B	25B	4BB	4CB	5GB	6MB	7UB	7TB	9_B	<pB	A�B	C�B	F�B	H�B	I�B	J�B	N�B	Q�B	TB	WB	YB	Z'B	[,B	\0B	]7B	]7B	^?B	aOB	c\B	dbB	dbB	egB	egB	fjB	hzB	i�B	i�B	j�B	j�B	l�B	o�B	r�B	t�B	v�B	v�B	x�B	z�B	|�B	�B	�B	�B	�,B	�7B	�=B	�IB	�OB	�\B	�cB	�uB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�&B	�,B	�+B	�2B	�3B	�DB	�GB	�RB	�ZB	�dB	�vB	�B	��B	�B	��B	B	ÚB	ǱB	ȷB	ȸB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�&B	�8B	�?B	�CB	�OB	�^B	�iB	�gB	�hB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 B
B
B
B
B
B
B
B
B
B
!B
)B
&B
*B
'B
(B
-B
0B
4B
3B
3B
4B
	:B
	7B
	;B
FB
RB
�B
�B
&�B
+B
36B
;eB
?B
F�B
L�B
Q�B
XB
^5B
bQB
fgB
jB
m�B
q�B
v�B
z�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.03 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214282016053112142820160531121428  AO  ARCAADJP                                                                    20140721230518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230518  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230518  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121428  IP                  G�O�G�O�G�O�                