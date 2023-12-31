CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-16T17:02:05Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20171016170205  20190405100808  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�.'��
1   @�.(j1a�@,�A�7K��d�z�H1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�  B�33B�  B���B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`�Ca��Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dys3D��D�S3D��3D���D�	�D�FfD��3D���D��D�@ D�ffD�ɚD�3D�S3DچfD��D� D�@ D�fD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��A��
B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B�(�B���B���B�(�B���B�(�B���B�B���B���B�B���B���B���C z�Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4z�C6z�C8z�C:z�C<z�C>z�C@z�CBz�CD�{CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^�{C`�{CbG�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�J>C�=qC�=qC�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL%DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy��D�)D�b�D���D��)D��D�U�D���D���D�(�D�O\D�u�D���D�"�D�b�Dڕ�D�)D�\D�O\D��D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A�^A�^A�jA�jA�jA�wA�wA�wA���A���A���A�A�A�ĜA�ƨA���A���A���A���A�ȴA�ȴA�ȴA�ȴA�ƨA�ȴA���A���A���A���A���A���A�ȴA�A�A�-A��7A�  A��mAק�A�p�A�7LA��`A�n�AԋDAӗ�Aқ�A�VAѥ�Aљ�A�A�A�%AЬA��A̕�A�I�A�I�A�G�A�VA�"�AÛ�A�A��A��A���A���A��A�v�A��#A�t�A��A��#A���A��^A�VA�?}A�Q�A�~�A���A�VA��jA���A�r�A�?}A���A�n�A�bA��A��A��7A� �A�5?A�z�A�?}A��wA�v�A�bA��uA�
=A�"�A���A���A�5?A�bNA��A�;dA��A���A��A��HA��A{/AiO�AdI�Ab�Ab$�AYoAO�FAN�HAN�DAM��AL�DAKx�AJ5?AH(�AC��A@A�A?�A>{A;��A8�A85?A6M�A5l�A5K�A57LA5VA4�A37LA2�A1�;A1�A/��A/|�A/33A/VA/&�A.�A.�A.��A.��A.��A.1A-�A-7LA-A,��A,(�A+�A+l�A+C�A*$�A)�PA)/A(��A(�A(9XA'"�A%�mA%?}A%�A%A$��A$JA#+A"��A"VA"�A!�hA!K�A ��A I�A��AVA�A%A��A-A�TA�7AO�A�AbNA�A��AG�A33A&�A&�A"�A�A��Az�AE�A9XA��AbNA"�AA�`A��A�\AA"�A��A�AƨAO�AjA�^AhsA�A��A��A�A1Ap�A�jA^5A�mA��A�At�AdZA?}A
��A
~�A
 �A	��A	p�A	
=A��AJA�PA`BA&�A��AbNA1A��A`BA��AM�A(�AA�mA��AȴA�AG�A ��A �A �jA z�A (�@���@��@���@�%@�b@���@���@�M�@��@�x�@�O�@�O�@�O�@�X@�X@�V@�I�@���@�K�@��@��-@���@�@�(�@�F@�33@��@�!@�+@�E�@��@�-@�x�@�V@��@�I�@��@��@�\)@�\@���@�R@�J@�/@�I�@� �@�1@�;d@�`B@�@�I�@��;@�^5@��@��@�r�@�A�@���@�l�@ޟ�@��T@ݩ�@ݙ�@�G�@��@ܬ@��m@ڇ+@�@��@؋D@ץ�@�=q@��@ԓu@�C�@мj@�j@�(�@�b@�l�@��H@�-@��@˥�@�v�@�7L@��@�O�@���@ēu@�b@��
@å�@ÍP@�33@°!@�~�@��^@�/@�(�@�l�@�C�@�"�@�
=@��@��\@���@�?}@�Ĝ@��u@�z�@�Z@�9X@� �@�l�@�o@���@��@���@�ȴ@�^5@��@��T@��T@��T@��#@���@��@�Z@��@���@�;d@��!@�7L@��u@�bN@�A�@�1'@��@�  @��@��
@���@�ƨ@��w@���@���@��@�dZ@�S�@�S�@�S�@�S�@�\)@��@��+@�5?@�@���@�&�@�z�@��@�l�@�E�@���@�O�@�1'@�V@��@��@�V@��@���@�bN@�  @���@�K�@�
=@���@��!@�v�@��@��@��9@�z�@�j@���@�+@���@�{@���@��h@��@��/@���@�j@��
@�\)@�v�@�$�@��@��h@�G�@��@���@���@�9X@� �@��@��@��m@���@��@��!@��H@�ȴ@��\@�ff@�@��h@�hs@�%@�-@���@�o@��
@�bN@{��@rM�@e�@Y�7@N@CS�@:-@3��@-�@(�`@%?}@#33@5?@�H@�@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A�^A�^A�jA�jA�jA�wA�wA�wA���A���A���A�A�A�ĜA�ƨA���A���A���A���A�ȴA�ȴA�ȴA�ȴA�ƨA�ȴA���A���A���A���A���A���A�ȴA�A�A�-A��7A�  A��mAק�A�p�A�7LA��`A�n�AԋDAӗ�Aқ�A�VAѥ�Aљ�A�A�A�%AЬA��A̕�A�I�A�I�A�G�A�VA�"�AÛ�A�A��A��A���A���A��A�v�A��#A�t�A��A��#A���A��^A�VA�?}A�Q�A�~�A���A�VA��jA���A�r�A�?}A���A�n�A�bA��A��A��7A� �A�5?A�z�A�?}A��wA�v�A�bA��uA�
=A�"�A���A���A�5?A�bNA��A�;dA��A���A��A��HA��A{/AiO�AdI�Ab�Ab$�AYoAO�FAN�HAN�DAM��AL�DAKx�AJ5?AH(�AC��A@A�A?�A>{A;��A8�A85?A6M�A5l�A5K�A57LA5VA4�A37LA2�A1�;A1�A/��A/|�A/33A/VA/&�A.�A.�A.��A.��A.��A.1A-�A-7LA-A,��A,(�A+�A+l�A+C�A*$�A)�PA)/A(��A(�A(9XA'"�A%�mA%?}A%�A%A$��A$JA#+A"��A"VA"�A!�hA!K�A ��A I�A��AVA�A%A��A-A�TA�7AO�A�AbNA�A��AG�A33A&�A&�A"�A�A��Az�AE�A9XA��AbNA"�AA�`A��A�\AA"�A��A�AƨAO�AjA�^AhsA�A��A��A�A1Ap�A�jA^5A�mA��A�At�AdZA?}A
��A
~�A
 �A	��A	p�A	
=A��AJA�PA`BA&�A��AbNA1A��A`BA��AM�A(�AA�mA��AȴA�AG�A ��A �A �jA z�A (�@���@��@���@�%@�b@���@���@�M�@��@�x�@�O�@�O�@�O�@�X@�X@�V@�I�@���@�K�@��@��-@���@�@�(�@�F@�33@��@�!@�+@�E�@��@�-@�x�@�V@��@�I�@��@��@�\)@�\@���@�R@�J@�/@�I�@� �@�1@�;d@�`B@�@�I�@��;@�^5@��@��@�r�@�A�@���@�l�@ޟ�@��T@ݩ�@ݙ�@�G�@��@ܬ@��m@ڇ+@�@��@؋D@ץ�@�=q@��@ԓu@�C�@мj@�j@�(�@�b@�l�@��H@�-@��@˥�@�v�@�7L@��@�O�@���@ēu@�b@��
@å�@ÍP@�33@°!@�~�@��^@�/@�(�@�l�@�C�@�"�@�
=@��@��\@���@�?}@�Ĝ@��u@�z�@�Z@�9X@� �@�l�@�o@���@��@���@�ȴ@�^5@��@��T@��T@��T@��#@���@��@�Z@��@���@�;d@��!@�7L@��u@�bN@�A�@�1'@��@�  @��@��
@���@�ƨ@��w@���@���@��@�dZ@�S�@�S�@�S�@�S�@�\)@��@��+@�5?@�@���@�&�@�z�@��@�l�@�E�@���@�O�@�1'@�V@��@��@�V@��@���@�bN@�  @���@�K�@�
=@���@��!@�v�@��@��@��9@�z�@�j@���@�+@���@�{@���@��h@��@��/@���@�j@��
@�\)@�v�@�$�@��@��h@�G�@��@���@���@�9X@� �@��@��@��m@���@��@��!@��H@�ȴ@��\@�ff@�@��h@�hs@�%@�-@���@�o@��
@�bN@{��@rM�@e�@Y�7@N@CS�@:-@3��@-�@(�`@%?}@#33@5?@�H@�@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
XB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
W
B
VB
W
B
W
B
VB
VB
VB
VB
VB
VB
VB
VB
VB
VB
T�B
T�B
R�B
N�B
B�B	��B	�B	�;B	�B	�B
B
�B
49B
O�B
e`B
u�B
v�B
�B
�%B
�PB
ĜB
�TB+B.BH�BZBn�B�JB��B��B�'B�wB�B��BB
=BJB�B�B�B�B �B'�B.B;dB?}BA�BB�BA�BA�B>wB?}B>wB5?B"�B�B+B��B�B��B�9B��Bn�BD�B%�BhB
��B
��B
�B
�B
�B
�3B
�B
��B
��B
�+B
e`B
hB	�FB	^5B	J�B	D�B	9XB	�B	VB	DB	DB		7B	1B	+B	B	B	
=B	�B	�B	�B	,B	>wB	>wB	Q�B	[#B	]/B	]/B	`BB	jB	�B	�VB	�hB	��B	�B	�!B	�?B	�dB	ɺB	�B	�B	�#B	�)B	�BB	�;B	�NB	�B	�B
%B
�B
(�B
-B
.B
2-B
2-B
2-B
33B
33B
33B
7LB
9XB
:^B
9XB
9XB
:^B
:^B
:^B
9XB
9XB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
=qB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
=qB
<jB
<jB
<jB
<jB
;dB
<jB
>wB
>wB
>wB
=qB
=qB
=qB
=qB
=qB
>wB
=qB
<jB
<jB
<jB
;dB
;dB
;dB
;dB
;dB
:^B
:^B
9XB
9XB
9XB
8RB
8RB
8RB
7LB
7LB
6FB
5?B
5?B
5?B
33B
1'B
0!B
-B
+B
)�B
)�B
+B
(�B
'�B
%�B
$�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
oB
hB
hB
bB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
{B
{B
uB
uB
oB
oB
hB
hB
bB
bB
\B
PB
DB
1B
+B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
DB
DB
DB

=B
DB
DB

=B
	7B
1B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�fB	�`B	�mB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
%B
+B
1B
	7B
	7B

=B

=B

=B

=B
JB
JB
JB
DB
DB
DB
DB
JB
PB
VB
\B
\B
\B
\B
bB
bB
bB
\B
\B
\B
bB
bB
\B
\B
VB
VB
VB
VB
VB
VB
bB
oB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
%�B
/B
6FB
9XB
<jB
C�B
J�B
N�B
XB
_;B
e`B
iyB
m�B
p�B
r�B
w�B
y�B
|�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
W�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
V�B
U�B
V�B
V�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
T�B
T�B
R�B
N�B
BlB	��B	��B	�B	�]B	�B
�B
�B
4B
O�B
e9B
u�B
v�B
��B
��B
�*B
�vB
�,BB-�BH�BY�BnpB�#B��B��B��B�OB��B��B�B
B B^B�B�B�B �B'�B-�B;7B?QBAcBBgBAbBAbB>NB?QB>JB5B"�BYB�B��B�ZB̠B�B�_BnkBDnB%�B<B
��B
�B
�B
�YB
��B
�B
��B
��B
��B
��B
e1B
5B	�B	^B	J�B	DiB	9%B	rB	#B	B	B		B	�B	�B	�B	�B	
B	QB	lB	�B	+�B	>?B	>@B	Q�B	Z�B	\�B	\�B	`B	jIB	��B	�!B	�1B	�\B	��B	��B	�B	�.B	ɂB	��B	��B	��B	��B	�B	�B	�B	�UB	�lB
�B
yB
(�B
,�B
-�B
1�B
1�B
1�B
2�B
2�B
2�B
7B
9 B
:'B
9"B
9 B
:&B
:&B
:'B
9 B
9"B
8B
9 B
:'B
:'B
;,B
;*B
;+B
<2B
=7B
<2B
<2B
<1B
<3B
</B
<2B
<.B
=9B
=6B
=:B
=7B
=:B
=8B
=9B
=8B
<2B
<2B
<2B
<3B
;(B
</B
>;B
>>B
>=B
=9B
=7B
=5B
=9B
=9B
>@B
=5B
<1B
<1B
<1B
;+B
;*B
;)B
;,B
;,B
:#B
:"B
9B
9B
9B
8B
8B
8B
7B
7B
6B
5B
5B
5B
2�B
0�B
/�B
,�B
*�B
)�B
)�B
*�B
(�B
'�B
%�B
$�B
"�B
!�B
 �B
�B
}B
pB
^B
NB
IB
EB
?B
@B
9B
;B
4B
.B
,B
'B
@B
QB
WB
XB
WB
YB
YB
VB
UB
VB
YB
VB
QB
LB
DB
NB
LB
KB
HB
GB
?B
?B
?B
BB
AB
>B
;B
;B
5B
4B
-B
-B
%B
(B
!B
B

B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
B

B

 B
	B
B
	�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�mB	�UB	�DB	�'B	�#B	�0B	�2B	�BB	�GB	�GB	�MB	�LB	�\B	�TB	�aB	�aB	�gB	�mB	�mB	�nB	�lB	�mB	�mB	�vB	�rB	�vB	�yB	�wB	�xB	�xB	�yB	�B	�~B	�~B	�B	�B	�}B	�B	�B	�~B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
	�B
	�B
	�B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
$B
%B
#B
B
B
B
#B
#B
B
B
B
B
B
B
B
B
$B
0B
0B
6B
CB
CB
HB
DB
BB
BB
DB
DB
DB
BB
CB
;B
BB
GB
ZB
`B
_B
[B
hB
iB
sB
}B
%�B
%�B
.�B
6B
9B
<+B
CWB
J�B
N�B
W�B
^�B
e"B
i;B
mRB
pcB
rrB
w�B
y�B
|�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.48 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008082019040510080820190405100808  AO  ARCAADJP                                                                    20171016170205    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171016170205  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171016170205  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100808  IP                  G�O�G�O�G�O�                