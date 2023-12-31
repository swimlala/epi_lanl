CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:28Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125828  20190408133246  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @��I�l|�1   @��J\�(�@4�vȴ9X�b���E�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D��fD�L�D�ffD��fD�fD�VfD���D���D��3D�@ D��fDǼ�D��D�FfD�vfD��fD���D�,�D�\�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
=@��
A�A!�AA�Aa�A���A���A���A���A���A���A���A���B z�Bz�Bz�Bz�B z�B(z�B0�GB8z�B@z�BHz�BPz�BXz�B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C�C�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C08RC2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch8RCj8RCl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy��D��=D�P�D�j=D��=D�
=D�Z=D��qD�ФD��
D�C�D��=D���D�qD�J=D�z=D��=D� �D�0�D�`�D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A��A�$�A�&�A�$�A�$�A�"�A�"�A� �A�
=A���A�r�A� �A��A���A���A���A�9XA��AͅA�7LA�A̶FA�Q�A���A˴9A˾wA�VAʣ�A���A�K�AǼjAƏ\A���A²-A��uA�A�5?A�
=A�ƨA��A���A���A��+A�`BA�
=A�E�A�"�A��9A�p�A�(�A���A��FA�p�A�%A�1A��FA�33A�M�A�A��A�z�A�K�A�5?A��#A�VA�{A�VA��DA��A�1A���A��A���A��jA�r�A��A�r�A�oA�G�A��A�l�A�A�~�A��FA�hsA��FA���A�7LA��!A�A���A��PA��RA�=qA�=qA���A��hA�Q�A���A�^5A��A�t�A�$�A��A��#A���A��#A��A��A�jA�(�A�x�A���A�33A�%A�JA��-A�ffA�PA{O�At(�Ar�RAr$�Ao��Ak;dAi?}Ae��Abv�A`�A]�
A[��AX�+AT��ATJAQXAN�!AM33AL�DAJ��AI/AHE�AE��AD�AD�!ADQ�AAx�A?7LA=�A<$�A:�A8jA7�A3�TA1�#A/dZA-O�A,I�A+�A)A(VA'�A&�yA&^5A%�A$��A#;dA"r�A!��A!K�A z�AVA�A �A\)AA��A�-AA��AbNA&�Av�AbAA�A^5A��A�wA
=A�A�A�A��AXA
��A
 �A	33A�mA�hA��A�Ap�A�A�A=qAVA�AhsA M�@�@�@�I�@�C�@�/@�Q�@�\)@�M�@�1@�@�=q@�@��9@���@�Ĝ@�P@�p�@�@�33@�E�@�&�@�  @�+@�^5@���@�&�@�@��
@�ff@���@��@�G�@��@�1@��
@�\)@◍@�$�@�`B@�\)@���@�v�@���@݁@�p�@�&�@��/@ܓu@ۥ�@��y@�o@ڧ�@�X@��y@��y@�
=@��@ԓu@��@Ѻ^@�O�@�Ĝ@�bN@ύP@·+@��@͉7@���@�ff@�~�@���@�5?@�b@�1'@�J@�O�@�X@���@���@�M�@���@��@���@��@��@�t�@�"�@�5?@�@��@��T@��@��T@���@���@���@���@���@��@���@�;d@��y@��@���@��-@��9@�A�@�r�@��D@�z�@�9X@���@��@�33@���@��H@�ȴ@���@�V@�-@�$�@���@���@�x�@�X@�V@��9@�j@�A�@�b@��m@��w@��@��@�l�@�+@�
=@���@�ff@�-@�@�@��7@�O�@�&�@��9@��
@���@��P@�o@�V@�-@���@�V@�z�@��;@�  @���@��@�S�@��y@���@��R@��H@���@���@���@�G�@�x�@�/@�&�@�/@�/@���@��@�I�@���@���@�C�@�C�@�;d@��H@�ȴ@��!@�ff@�V@�{@��9@��j@�z�@���@���@��7@�Q�@�(�@�  @��@��@���@���@�5?@�$�@�ȴ@��@���@��#@��@�E�@�V@�V@�V@���@�ff@�5?@��T@��h@�p�@�p�@�/@��@��/@��@�z�@�b@��m@��;@��w@��P@�S�@�"�@��@�@�
=@�33@�S�@�\)@�dZ@�l�@�ȴ@���@���@�hs@�X@��@�%@���@���@��@��j@�A�@�1@���@�C�@�
=@��@�ȴ@��R@�ff@��@��#@��^@�O�@�&�@���@�Z@�b@��@���@�t�@�K�@�@��H@���@��@��@���@�~�@�5?@��@�/@��F@��!@z�H@q�@ko@b�@Y��@RJ@L1@E/@>ȴ@9��@3S�@+ƨ@&ff@!x�@"�@�y@C�@
=@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�{A��A�$�A�&�A�$�A�$�A�"�A�"�A� �A�
=A���A�r�A� �A��A���A���A���A�9XA��AͅA�7LA�A̶FA�Q�A���A˴9A˾wA�VAʣ�A���A�K�AǼjAƏ\A���A²-A��uA�A�5?A�
=A�ƨA��A���A���A��+A�`BA�
=A�E�A�"�A��9A�p�A�(�A���A��FA�p�A�%A�1A��FA�33A�M�A�A��A�z�A�K�A�5?A��#A�VA�{A�VA��DA��A�1A���A��A���A��jA�r�A��A�r�A�oA�G�A��A�l�A�A�~�A��FA�hsA��FA���A�7LA��!A�A���A��PA��RA�=qA�=qA���A��hA�Q�A���A�^5A��A�t�A�$�A��A��#A���A��#A��A��A�jA�(�A�x�A���A�33A�%A�JA��-A�ffA�PA{O�At(�Ar�RAr$�Ao��Ak;dAi?}Ae��Abv�A`�A]�
A[��AX�+AT��ATJAQXAN�!AM33AL�DAJ��AI/AHE�AE��AD�AD�!ADQ�AAx�A?7LA=�A<$�A:�A8jA7�A3�TA1�#A/dZA-O�A,I�A+�A)A(VA'�A&�yA&^5A%�A$��A#;dA"r�A!��A!K�A z�AVA�A �A\)AA��A�-AA��AbNA&�Av�AbAA�A^5A��A�wA
=A�A�A�A��AXA
��A
 �A	33A�mA�hA��A�Ap�A�A�A=qAVA�AhsA M�@�@�@�I�@�C�@�/@�Q�@�\)@�M�@�1@�@�=q@�@��9@���@�Ĝ@�P@�p�@�@�33@�E�@�&�@�  @�+@�^5@���@�&�@�@��
@�ff@���@��@�G�@��@�1@��
@�\)@◍@�$�@�`B@�\)@���@�v�@���@݁@�p�@�&�@��/@ܓu@ۥ�@��y@�o@ڧ�@�X@��y@��y@�
=@��@ԓu@��@Ѻ^@�O�@�Ĝ@�bN@ύP@·+@��@͉7@���@�ff@�~�@���@�5?@�b@�1'@�J@�O�@�X@���@���@�M�@���@��@���@��@��@�t�@�"�@�5?@�@��@��T@��@��T@���@���@���@���@���@��@���@�;d@��y@��@���@��-@��9@�A�@�r�@��D@�z�@�9X@���@��@�33@���@��H@�ȴ@���@�V@�-@�$�@���@���@�x�@�X@�V@��9@�j@�A�@�b@��m@��w@��@��@�l�@�+@�
=@���@�ff@�-@�@�@��7@�O�@�&�@��9@��
@���@��P@�o@�V@�-@���@�V@�z�@��;@�  @���@��@�S�@��y@���@��R@��H@���@���@���@�G�@�x�@�/@�&�@�/@�/@���@��@�I�@���@���@�C�@�C�@�;d@��H@�ȴ@��!@�ff@�V@�{@��9@��j@�z�@���@���@��7@�Q�@�(�@�  @��@��@���@���@�5?@�$�@�ȴ@��@���@��#@��@�E�@�V@�V@�V@���@�ff@�5?@��T@��h@�p�@�p�@�/@��@��/@��@�z�@�b@��m@��;@��w@��P@�S�@�"�@��@�@�
=@�33@�S�@�\)@�dZ@�l�@�ȴ@���@���@�hs@�X@��@�%@���@���@��@��j@�A�@�1@���@�C�@�
=@��@�ȴ@��R@�ff@��@��#@��^@�O�@�&�@���@�Z@�b@��@���@�t�@�K�@�@��H@���@��@��@���@�~�@�5?@��G�O�@��F@��!@z�H@q�@ko@b�@Y��@RJ@L1@E/@>ȴ@9��@3S�@+ƨ@&ff@!x�@"�@�y@C�@
=@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
t�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
u�B
}�B
�1B
��B
��B
��B
��B
��B
�RB
B
��B
�;B
�yB
��B%B�B.BJ�BhsB�1B��BƨB+B>wBI�B#�B�B�`B�B��BB{B%�B8RBG�BR�B^5Bs�Bz�B�+B�DB�hB��B�B�wB�dB�?B�{B{�Bk�BcTBZBO�BK�BH�BA�B/B)�BaHB� B�-B��B��BĜBB��B�dB�'B��B�JB� By�Br�Bl�BffB`BBR�BI�B?}B2-B�B{BPB��B�yB�BB��BɺBB��B�BjBG�B8RB�B
�B
ɺB
�B
�B�BB
��B
�B
�#B
��B
�B
�B
�?B
�hB
E�B
"�B	�B	�LB	�B	�B	��B	�B	w�B	bNB	O�B	C�B	1'B	!�B	\B��B��B�B�BB�B��B��BɺBĜB�wB�^B�RB�?B�'B��B��B��B��B��B��B��B�hB�VB�7B�+B}�By�Bx�B~�B�B�=B�PB�7B�B�B� B�B�Bx�Bs�Bs�Bo�BjBiyBffBdZBcTB[#BXBXBYB`BBhsB^5B[#BZBYBXBW
BW
BW
BXBXBW
BS�BS�BR�BR�BS�B[#BgmBiyBhsBgmBiyBhsBhsBl�Bv�Bz�B{�B|�B{�B|�By�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�9B�3B�9B�LB�RB�dBĜBȴBɺB��B��B��B�B�
B�B�B�)B�5B�BB�)B��B�5B�NB�BB�5B�/B�BB�TB�`B�`B�mB�B�B�B�B�B�NB�yB�B�`B�B�B�B�B��B��B	B��B��B��B	B	B	B	+B	DB	DB	JB	VB	\B	{B	�B	%�B	(�B	+B	-B	7LB	:^B	;dB	=qB	>wB	A�B	A�B	@�B	@�B	D�B	I�B	K�B	L�B	M�B	P�B	W
B	ZB	[#B	\)B	]/B	^5B	aHB	bNB	cTB	dZB	ffB	gmB	jB	m�B	o�B	p�B	q�B	r�B	r�B	s�B	s�B	s�B	u�B	w�B	y�B	z�B	{�B	}�B	� B	�B	�B	�B	�%B	�+B	�1B	�JB	�VB	�VB	�VB	�\B	�\B	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�!B	�'B	�'B	�'B	�'B	�-B	�3B	�9B	�?B	�?B	�FB	�3B	�?B	�9B	�-B	�!B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�9B	�?B	�FB	�FB	�LB	�RB	�XB	�^B	�jB	�jB	�qB	�qB	�wB	�}B	��B	��B	B	ĜB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�)B	�)B	�)B	�5B	�BB	�HB	�TB	�ZB	�ZB	�fB	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B
+B	��B
1B
\B
�B
�B
%�B
/B
5?B
:^B
>wB
C�B
I�B
O�B
VB
[#B
`BB
ffB
jB
m�B
q�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
t�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
u�B
}�B
�(B
�uB
��B
��B
��B
��B
�FB
B
��B
�2B
�oB
��BBxB.BJ�BhhB�&B��BƠBB>lBI�B#�B�B�XB�B��B	BpB%�B8GBG�BR�B^)Bs�Bz�B�"B�<B�\B��B�B�kB�ZB�0B�pB{�Bk{BcJBZBO�BK�BH�BA}B/B)�Ba;B�B� B��B��BēBB�}B�XB�B��B�=B�By�Br�BlBfYB`8BR�BI�B?tB2#B�BqBEB��B�mB�6B��BɮBB��B�BjuBG�B8JB�B
�B
ɪB
�B
�yB~BB
��B
�B
�B
��B
� B
�B
�3B
�]B
E�B
"�B	�B	�@B	�	B	�B	��B	�B	w�B	b@B	O�B	C�B	1B	!�B	NB��B��B�qB�4B�B��B��BɬBďB�jB�QB�CB�1B�B��B��B��B��B��B��B�yB�\B�HB�*B�B}�By�Bx�B~�B�B�0B�BB�)B�B��B�B��B��Bx�Bs�Bs�Bo�BjoBijBfYBdJBcGB[BXBXBYB`4BheB^'B[BZBY	BXBV�BV�BV�BX BXBV�BS�BS�BR�BR�BS�B[Bg`BilBhcBg]BiiBhfBhdBlzBv�Bz�B{�B|�B{�B|�By�B�	B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B� B�B�B�*B�%B�+B�<B�CB�VBďBȤBɭB˶B��B��B��B��B� B�B�B�'B�3B�B��B�'B�?B�3B�$B�!B�4B�GB�OB�PB�_B�nB�B�B�B�|B�>B�iB�uB�RB�B�vB�tB�B��B��B	 �B��B��B��B	B	 �B	B	B	3B	6B	;B	HB	MB	lB	�B	%�B	(�B	*�B	,�B	7<B	:NB	;SB	=aB	>gB	AvB	AxB	@sB	@tB	D�B	I�B	K�B	L�B	M�B	P�B	V�B	ZB	[B	\B	] B	^'B	a7B	b?B	cFB	dKB	fTB	g\B	jpB	m�B	o�B	p�B	q�B	r�B	r�B	s�B	s�B	s�B	u�B	w�B	y�B	z�B	{�B	}�B	�B	��B	��B	�
B	�B	�B	�B	�9B	�EB	�FB	�GB	�LB	�NB	�KB	�VB	�kB	�wB	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�.B	�.B	�6B	�$B	�/B	�+B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�(B	�.B	�6B	�8B	�>B	�BB	�IB	�MB	�ZB	�YB	�_B	�`B	�gB	�oB	�sB	�xB	�}B	ČB	ŒB	ƘB	ǠB	ɩB	˷B	̻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�/B	�8B	�DB	�JB	�IB	�WB	�YB	�^B	�kB	�oB	�oB	�uB	�yB	�|B	�B	�B	�B	�B	�B	��B	��B	��B	��G�O�B	��B
 B
LB
pB
�B
%�B
/B
5.B
:NB
>fB
C�B
I�B
O�B
U�B
[B
`3B
fVB
jnB
m�B
q�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332462019040813324620190408133246  AO  ARCAADJP                                                                    20181121125828    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125828  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125828  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133246  IP                  G�O�G�O�G�O�                