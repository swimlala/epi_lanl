CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-05-27T02:15:32Z AOML 3.0 creation; 2016-08-07T21:36:47Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160527021532  20160825183352  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               xA   AO  5286_8897_120                   2C  D   APEX                            6531                            072314                          846 @ׯ=��T1   @ׯ>@y{�@4�7Kƨ�cA\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    xA   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D�fD�P D�vfD�ɚD�	�D�9�D���D��fD��D�P D�p D�� D�3D�6fDڃ3D���D�fD�P D�|�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @AG�@��
@��
A�A!�AA�Aa�A���A���A���A���A���A���A���A���B �GBz�Bz�B{B z�B(z�B0z�B8z�B@z�BHz�BPz�BXz�B`z�Bhz�Bpz�Bxz�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC �C�C�C�C�C
�C�C�C8RC�C�C�C�C�C�C�C �C"�C$�C&�C(�C*�C,�C.�C0�C2�C4�C6�C8�C:�C<�C>�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb�Cd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�)C�)C�\C�)C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\D �D ��D�D��DHD��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DPDP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtg�DyǮD�=D�S�D�z=D��qD�qD�=qD���D��=D��D�S�D�s�D���D�
D�:=Dڇ
D��D�
=D�S�D�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A���A͑hA�\)A��A���A��A��mA��`A��TA��/A��A��#A��#A���A���A�ƨA�x�A��;A���A�7LA��TAǑhA�jA�&�A��;Aś�A�;dA��`Aĝ�A�"�AîA�~�A�ZA�A��A�x�A���A�JA�33A���A��uA���A���A�K�A�33A�C�A���A���A��A�VA��DA��A���A�ȴA��A�A�A��
A�S�A��A�|�A��A��wA���A�dZA��
A��7A�Q�A��A�VA�oA�K�A��A��^A���A�"�A��9A�t�A��\A���A�ȴA��A�{A��PA��A��+A�A�A��A���A�x�A��A�(�A���A��uA��;A��A�S�A�(�A
=A~�A}x�A{%Av��Aq33Ao;dAn �Am+Aj�+Ag`BAe|�Adv�Ac��Ab��A_�wA[AZI�AXv�AV��ASS�AQ��AO�TAO%AMXAK�AK�AJA�AIt�AIAG�^AEACp�AA`BA@��A@(�A>�A=?}A;��A;+A:{A8A5��A5�A4{A3��A3K�A2ȴA1�wA1G�A0�RA/��A.1'A,v�A*bA(�HA'��A'�A%?}A#�A"1A!"�A  �A��A�AG�Az�A�AS�A��A(�A��A��A�AXA�!A�uA�A%A��A^5A�FAĜA��A1'Ar�A|�A33A��A~�A�^AG�A+A
r�A	�A	p�A��A�A��A�A`BAffA�-AK�A�A�`A��A�+A��A�Ax�A
=A -A =qA 5?A $�@��@�p�@�G�@��D@��;@��9@�Z@�@�"�@�ff@�7L@���@��H@���@�@���@�@��@�r�@��@�^5@�@�j@�w@�t�@�K�@���@�5?@��@�7@���@�%@�bN@�b@��m@��@�5?@�~�@�=q@ݙ�@���@ܼj@܃@�\)@��@ڟ�@�V@��y@���@�1'@��@Ѻ^@Гu@�t�@�hs@�bN@ˍP@�ȴ@�J@�/@�l�@�v�@���@�p�@�/@Ĭ@Å@���@���@��@���@��@�x�@�Z@�1'@�v�@�X@��@�;d@�-@���@��7@�7L@���@�z�@��@���@���@�K�@�@���@���@�G�@���@�r�@�9X@�1'@� �@�1@�C�@��!@�v�@�5?@�X@��@���@�+@�ff@��#@��^@���@��@�X@�?}@�%@��@��@�Z@�+@��y@��!@�~�@�^5@�E�@��@���@�G�@��9@��;@��F@���@���@�|�@�l�@�K�@��H@�~�@�n�@�-@��#@���@���@��7@�x�@�?}@��@��/@���@���@�z�@�(�@��m@��w@��@���@�K�@�33@���@�n�@�V@�5?@��@��-@�p�@�?}@���@��@�Z@�(�@�1@��F@�\)@�o@��H@��\@�-@�=q@�@��h@��@�`B@���@��@��
@���@�C�@�~�@�$�@��h@�&�@���@��9@�z�@�Z@� �@��
@�|�@�
=@�n�@�-@��@���@�G�@��@��u@�r�@��@���@��@��@��w@�ƨ@��@�;d@�t�@��P@��P@��@�t�@�S�@�33@�33@�;d@�C�@�+@��H@��!@�~�@��T@���@�hs@�/@��@��/@��@�z�@�Z@� �@�1@��
@��@�@��!@�~�@�ff@�^5@�V@�E�@�-@��@���@�7L@���@��@��@�bN@�Z@�9X@�  @���@�33@��R@�^5@�J@��T@���@�x�@�G�@�/@��@��@��@��`@�Ĝ@�o@~ȴ@z�@o�;@h  @`��@X1'@R^5@K�m@B�!@:�@4��@.��@*-@%�T@�@(�@��@p�@�^@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A���A͑hA�\)A��A���A��A��mA��`A��TA��/A��A��#A��#A���A���A�ƨA�x�A��;A���A�7LA��TAǑhA�jA�&�A��;Aś�A�;dA��`Aĝ�A�"�AîA�~�A�ZA�A��A�x�A���A�JA�33A���A��uA���A���A�K�A�33A�C�A���A���A��A�VA��DA��A���A�ȴA��A�A�A��
A�S�A��A�|�A��A��wA���A�dZA��
A��7A�Q�A��A�VA�oA�K�A��A��^A���A�"�A��9A�t�A��\A���A�ȴA��A�{A��PA��A��+A�A�A��A���A�x�A��A�(�A���A��uA��;A��A�S�A�(�A
=A~�A}x�A{%Av��Aq33Ao;dAn �Am+Aj�+Ag`BAe|�Adv�Ac��Ab��A_�wA[AZI�AXv�AV��ASS�AQ��AO�TAO%AMXAK�AK�AJA�AIt�AIAG�^AEACp�AA`BA@��A@(�A>�A=?}A;��A;+A:{A8A5��A5�A4{A3��A3K�A2ȴA1�wA1G�A0�RA/��A.1'A,v�A*bA(�HA'��A'�A%?}A#�A"1A!"�A  �A��A�AG�Az�A�AS�A��A(�A��A��A�AXA�!A�uA�A%A��A^5A�FAĜA��A1'Ar�A|�A33A��A~�A�^AG�A+A
r�A	�A	p�A��A�A��A�A`BAffA�-AK�A�A�`A��A�+A��A�Ax�A
=A -A =qA 5?A $�@��@�p�@�G�@��D@��;@��9@�Z@�@�"�@�ff@�7L@���@��H@���@�@���@�@��@�r�@��@�^5@�@�j@�w@�t�@�K�@���@�5?@��@�7@���@�%@�bN@�b@��m@��@�5?@�~�@�=q@ݙ�@���@ܼj@܃@�\)@��@ڟ�@�V@��y@���@�1'@��@Ѻ^@Гu@�t�@�hs@�bN@ˍP@�ȴ@�J@�/@�l�@�v�@���@�p�@�/@Ĭ@Å@���@���@��@���@��@�x�@�Z@�1'@�v�@�X@��@�;d@�-@���@��7@�7L@���@�z�@��@���@���@�K�@�@���@���@�G�@���@�r�@�9X@�1'@� �@�1@�C�@��!@�v�@�5?@�X@��@���@�+@�ff@��#@��^@���@��@�X@�?}@�%@��@��@�Z@�+@��y@��!@�~�@�^5@�E�@��@���@�G�@��9@��;@��F@���@���@�|�@�l�@�K�@��H@�~�@�n�@�-@��#@���@���@��7@�x�@�?}@��@��/@���@���@�z�@�(�@��m@��w@��@���@�K�@�33@���@�n�@�V@�5?@��@��-@�p�@�?}@���@��@�Z@�(�@�1@��F@�\)@�o@��H@��\@�-@�=q@�@��h@��@�`B@���@��@��
@���@�C�@�~�@�$�@��h@�&�@���@��9@�z�@�Z@� �@��
@�|�@�
=@�n�@�-@��@���@�G�@��@��u@�r�@��@���@��@��@��w@�ƨ@��@�;d@�t�@��P@��P@��@�t�@�S�@�33@�33@�;d@�C�@�+@��H@��!@�~�@��T@���@�hs@�/@��@��/@��@�z�@�Z@� �@�1@��
@��@�@��!@�~�@�ff@�^5@�V@�E�@�-@��@���@�7L@���@��@��@�bN@�Z@�9X@�  @���@�33@��R@�^5@�J@��T@���@�x�@�G�@�/@��@��@��@��`G�O�@�o@~ȴ@z�@o�;@h  @`��@X1'@R^5@K�m@B�!@:�@4��@.��@*-@%�T@�@(�@��@p�@�^@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
e`B
e`B
dZB
e`B
dZB
cTB
cTB
bNB
cTB
cTB
dZB
cTB
dZB
e`B
ffB
e`B
gmB
hsB
w�B
��B�B/B7LB=qBYB^5BbNBgmBm�Br�Bt�Bx�B|�B|�B|�B�B�hB�uB��B��B��B�dB�jB�-B�B��B��B�B�LB��B��B��BPBK�B@�B �BB�B�yB�)B��B�B�DBy�B��B�dB�dB��B��B�-B��B�JBz�BcTBF�B-B!�BJB
��B  B�B{BJBB
��B
��B
��B
��B
�B
�B
�fB
ƨB
��B
��B
�VB
� B
gmB
L�B
9XB
.B
%�B
�B

=B	�sB	�}B	�!B	��B	��B	�bB	|�B	p�B	hsB	aHB	YB	>wB	+B	 �B	�B	
=B��B�B�fB�;B��B��B��BĜB�}B�jB�RB�!B��B��B��B��B�{B�bB�=B�+B�B�B~�B}�B}�B�B�B�DB�DB�DB�JB��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�'B�-B�3B�3B�3B�3B�'B�3B�dB�}B�qB��BÖBŢBĜBƨBǮBƨBÖB�jB�!B�B�B��B��B��B��B�B�!B�'B�'B�'B�3B�!B�-B�3B�?B�FB�FB�FB�LB�RB�RB�XB�dB��BŢB��B�B�
B�B�B�B�B�B�)B�)B�B�B�B��B��B��B��B��B��B��B�B�)B�5B�BB�TB�`B�sB�B�B��B��B��B��B��B	B	+B	1B	
=B	PB	oB	�B	�B	�B	"�B	(�B	-B	1'B	1'B	49B	5?B	6FB	8RB	9XB	9XB	;dB	<jB	=qB	C�B	D�B	F�B	G�B	J�B	K�B	K�B	K�B	L�B	O�B	R�B	S�B	T�B	YB	^5B	_;B	bNB	gmB	iyB	jB	k�B	l�B	n�B	o�B	q�B	s�B	u�B	u�B	v�B	w�B	w�B	w�B	x�B	y�B	{�B	}�B	~�B	�B	�+B	�7B	�DB	�JB	�VB	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�?B	�?B	�LB	�LB	�RB	�RB	�RB	�RB	�RB	�RB	�XB	�jB	��B	ÖB	ĜB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�)B	�/B	�BB	�HB	�NB	�TB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
+B
PB
oB
�B
!�B
&�B
.B
49B
9XB
?}B
G�B
L�B
R�B
XB
\)B
bNB
ffB
iyB
l�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
e\B
e\B
dVB
e^B
dUB
cOB
cPB
bLB
cPB
cOB
dWB
cPB
dVB
e_B
fbB
e[B
giB
hmB
w�B
ʼB�B/B7BB=iBYB^-BbFBgcBm�Br�Bt�Bx�B|�B|�B|�B�B�_B�nB�vB��B��B�[B�^B�#B�B��B��B��B�DB�xB�vB˹BHBK�B@rB �BB�B�mB�B��B�B�8By�B��B�XB�[B�zB�zB�#B��B�>Bz�BcLBF�B-B!�BBB
��B
��BzBpBCBB
��B
��B
��B
��B
�B
�B
�]B
ƢB
��B
�xB
�OB
�B
gfB
L�B
9TB
.B
%�B
�B

7B	�nB	�yB	� B	��B	��B	�aB	|�B	p�B	htB	aIB	YB	>{B	+B	 �B	�B	
>B��B�B�lB�?B�B��B��BġB��B�oB�ZB�)B��B��B��B��B��B�iB�DB�5B�&B�BB}�B}�B�!B�&B�NB�MB�NB�RB��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�1B�*B�4B�7B�8B�7B�7B�*B�8B�gB��B�tB��BÛBŧBġBƫBǯBƫBÙB�mB�&B�B�	B��B��B��B��B�B�%B�,B�,B�*B�:B�$B�1B�6B�CB�IB�IB�JB�NB�SB�UB�[B�fB��BťB��B�B�B�B�B�B�B�B�)B�+B�B�B�B��B��B��B��B��B��B��B�B�+B�8B�BB�VB�aB�uB�B�B��B��B��B��B��B	B	)B	0B	
=B	QB	oB	�B	�B	�B	"�B	(�B	-B	1$B	1%B	45B	5=B	6BB	8OB	9UB	9UB	;cB	<gB	=lB	C�B	D�B	F�B	G�B	J�B	K�B	K�B	K�B	L�B	O�B	R�B	S�B	T�B	YB	^1B	_8B	bIB	giB	itB	jzB	k�B	l�B	n�B	o�B	q�B	s�B	u�B	u�B	v�B	w�B	w�B	w�B	x�B	y�B	{�B	}�B	~�B	�	B	�&B	�0B	�>B	�DB	�QB	�[B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�-B	�0B	�9B	�8B	�GB	�EB	�JB	�JB	�JB	�JB	�MB	�JB	�PB	�aB	��B	ÎB	ĖB	řB	śB	ȪB	��B	��B	ʼB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�B	�B	�!B	�B	� B	�'B	�;B	�?B	�CB	�KB	�WB	�qB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
B
	B
	B
G�O�B
!B
EB
eB
�B
!�B
&�B
.
B
4/B
9NB
?rB
G�B
L�B
R�B
XB
\B
b@B
fZB
inB
l�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.12 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436472016080714364720160807143647  AO  ARCAADJP                                                                    20160527021532    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160527021532  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160527021532  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143647  IP                  G�O�G�O�G�O�                