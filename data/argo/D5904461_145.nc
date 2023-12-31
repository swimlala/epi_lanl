CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:30Z creation      
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125830  20190408133247  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @��18㦶1   @��1�B@4����m�b�KƧ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"y�D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtS3Dyy�D��D�@ D��fD�� D��D�L�D���D��fD�  D�P D�s3D�ɚD�3D�@ D�s3D���D���D�L�D�vfD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@��HAp�A!p�AAp�Aap�A��RA��RA��RA��A��RAиRA�RA�RB \)B\)B\)B\)B \)B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B���B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.C 
C
C
C
C
C

C
C
C
C
C
C
C
C
C
C
C 
C"
C$
C&
C(
C)�pC,
C.
C0
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CJ
CL
CN
CP
CR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"]D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�DtX�Dy]D�{D�B�D��GD���D��D�O�D��{D��GD�"�D�R�D�vD��{D�D�B�D�vD�ϮD���D�O�D�yGD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�%A�A�%A�%A�1A�1A�1A�
=A�
=A�JA�JA�r�A�5?AŇ+A��A�^5A��A�{A�"�A�t�A�A�O�A���A�~�A�C�A���A�ZA��hA��A�E�A���A�O�A�ĜA�XA��A�O�A�G�A�A�t�A�v�A���A��-A��wA�  A��+A�%A���A��jA�n�A�ZA�$�A��PA��mA���A��TA�K�A�$�A�z�A��A���A��A��HA�?}A�I�A�ffA��yA�%A�l�A���A��A�&�A��9A�?}A�ĜA��TA���A��RA�JA�A�A��A�VA�
=A��A���A�M�A�bA���A�p�A�VA~JA{�A{XAy"�AuoAtJAs�hArI�Ao��Ak�;AjJAhM�Ag�Ae�^Ad�AcG�A`�A_33A^�A\ȴA\bAY�AW7LAT��AP��AL�HAJ��AHA�AF�\ADĜAD~�AA��A?��A=��A;�A8��A7�A2�/A/A-��A,�HA+�-A*��A*5?A)�FA'�FA&�A'�A'�A'�A'%A(  A($�A&E�A$-A"��A!?}AƨA��A�FA;dA��A�mA�A7LA^5A�PA%A��A��A~�AI�Az�A1A&�A~�A�jA��A��AG�A%A�^AffA	ƨA�A�Ax�A1A��A5?A ��@�E�@�5?@�ȴ@�5?@�z�@��T@��@�ƨ@@�X@�j@��@�j@��@�o@��@���@�O�@�t�@��y@���@�x�@��u@ޟ�@�-@���@�dZ@�p�@��@؋D@�@���@ؓu@���@���@�r�@�&�@�&�@�%@�Z@��@�b@�  @��@�r�@�ƨ@�n�@���@�1@�dZ@�n�@���@�  @���@��@�^5@Ɂ@�V@�A�@�M�@�-@Ų-@�Z@�C�@�@��@�@�$�@���@�X@��@�b@��@���@��^@��^@��-@���@�?}@��D@�  @��w@�A�@��/@�Ĝ@��
@���@�O�@��@��9@�I�@��
@�S�@���@���@�Z@���@�
=@��@��@��u@��u@�bN@�l�@�(�@��^@�X@�G�@��y@�\)@�K�@���@�`B@���@��D@�Ĝ@���@��w@�A�@�b@�ƨ@���@�|�@�dZ@�K�@���@�^5@�$�@���@�x�@�x�@�hs@�%@���@�Q�@��@�|�@�S�@�o@�;d@���@�n�@���@��-@��-@���@�x�@���@�`B@�O�@�G�@�%@���@�bN@� �@���@�+@���@�n�@�5?@�=q@�{@��@���@�x�@��@���@�j@�(�@���@��
@��P@�C�@��@�o@��@��!@�V@�{@�@�x�@�G�@��@���@�I�@���@���@��@�t�@��y@���@�~�@�v�@�V@�M�@��@���@�x�@�O�@���@�  @�\)@�K�@�;d@�K�@�C�@��@��@���@�V@���@��-@�?}@��@���@�r�@��F@��@�\)@�dZ@�l�@�\)@�33@�o@��@�v�@��@�J@��T@�hs@�?}@�&�@��`@��@��u@�Z@� �@���@���@�l�@��@�@��y@�V@��@�%@��j@��9@�A�@�b@��w@�K�@���@�J@���@��h@�p�@�?}@���@��@�j@�Q�@�Q�@�Q�@�Q�@�1'@��
@�ƨ@��w@��F@���@�l�@�\)@�K�@��y@��\@�@�Ĝ@�x�@�G�@�`B@�X@��@���@�Ĝ@��/@�I�@z�!@q��@h�u@aX@Y%@R^5@Ko@BM�@;��@65?@.E�@)7L@$�D@  �@C�@ff@t�@ȴ@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�  A�A�A�A�A�A�A�%A�A�%A�%A�1A�1A�1A�
=A�
=A�JA�JA�r�A�5?AŇ+A��A�^5A��A�{A�"�A�t�A�A�O�A���A�~�A�C�A���A�ZA��hA��A�E�A���A�O�A�ĜA�XA��A�O�A�G�A�A�t�A�v�A���A��-A��wA�  A��+A�%A���A��jA�n�A�ZA�$�A��PA��mA���A��TA�K�A�$�A�z�A��A���A��A��HA�?}A�I�A�ffA��yA�%A�l�A���A��A�&�A��9A�?}A�ĜA��TA���A��RA�JA�A�A��A�VA�
=A��A���A�M�A�bA���A�p�A�VA~JA{�A{XAy"�AuoAtJAs�hArI�Ao��Ak�;AjJAhM�Ag�Ae�^Ad�AcG�A`�A_33A^�A\ȴA\bAY�AW7LAT��AP��AL�HAJ��AHA�AF�\ADĜAD~�AA��A?��A=��A;�A8��A7�A2�/A/A-��A,�HA+�-A*��A*5?A)�FA'�FA&�A'�A'�A'�A'%A(  A($�A&E�A$-A"��A!?}AƨA��A�FA;dA��A�mA�A7LA^5A�PA%A��A��A~�AI�Az�A1A&�A~�A�jA��A��AG�A%A�^AffA	ƨA�A�Ax�A1A��A5?A ��@�E�@�5?@�ȴ@�5?@�z�@��T@��@�ƨ@@�X@�j@��@�j@��@�o@��@���@�O�@�t�@��y@���@�x�@��u@ޟ�@�-@���@�dZ@�p�@��@؋D@�@���@ؓu@���@���@�r�@�&�@�&�@�%@�Z@��@�b@�  @��@�r�@�ƨ@�n�@���@�1@�dZ@�n�@���@�  @���@��@�^5@Ɂ@�V@�A�@�M�@�-@Ų-@�Z@�C�@�@��@�@�$�@���@�X@��@�b@��@���@��^@��^@��-@���@�?}@��D@�  @��w@�A�@��/@�Ĝ@��
@���@�O�@��@��9@�I�@��
@�S�@���@���@�Z@���@�
=@��@��@��u@��u@�bN@�l�@�(�@��^@�X@�G�@��y@�\)@�K�@���@�`B@���@��D@�Ĝ@���@��w@�A�@�b@�ƨ@���@�|�@�dZ@�K�@���@�^5@�$�@���@�x�@�x�@�hs@�%@���@�Q�@��@�|�@�S�@�o@�;d@���@�n�@���@��-@��-@���@�x�@���@�`B@�O�@�G�@�%@���@�bN@� �@���@�+@���@�n�@�5?@�=q@�{@��@���@�x�@��@���@�j@�(�@���@��
@��P@�C�@��@�o@��@��!@�V@�{@�@�x�@�G�@��@���@�I�@���@���@��@�t�@��y@���@�~�@�v�@�V@�M�@��@���@�x�@�O�@���@�  @�\)@�K�@�;d@�K�@�C�@��@��@���@�V@���@��-@�?}@��@���@�r�@��F@��@�\)@�dZ@�l�@�\)@�33@�o@��@�v�@��@�J@��T@�hs@�?}@�&�@��`@��@��u@�Z@� �@���@���@�l�@��@�@��y@�V@��@�%@��j@��9@�A�@�b@��w@�K�@���@�J@���@��h@�p�@�?}@���@��@�j@�Q�@�Q�@�Q�@�Q�@�1'@��
@�ƨ@��w@��F@���@�l�@�\)@�K�@��y@��\@�@�Ĝ@�x�@�G�@�`B@�X@��@���@�Ĝ@��/@�I�@z�!@q��@h�u@aX@Y%@R^5@Ko@BM�@;��@65?@.E�@)7L@$�D@  �@C�@ff@t�@ȴ@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBhsBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBiyBjBiyBiyBiyBffBiyB�#B  B%BJBPB�B�yB�;B\B"�B�B�B!�B6FBH�BT�BO�BM�BN�BhsB{�B�uB��B��B��B��B�+B�B�1B��B�B��B�B��B��B�bB|�B�oB��B��B�JB�+Bz�BbNBXBK�B33B�BVB��B�B�5B��Bz�Bm�BgmBW
BL�B\)B�B�Bw�BiyBhsBJ�B
��B
��B
�\B
'�B	��B	�TB	�;B	��B	ŢB	ȴB	B	�wB	�RB	ĜB	�/B	��B	�}B	�RB	�?B	�!B	��B	��B	�DB	�B	}�B	u�B	o�B	gmB	XB	N�B	I�B	;dB	1'B	�B	\B��B�5BɺB�qB�B��B��B��B�PB�Bw�Bk�B^5BP�B0!B�B�B�B�B �B!�B�B�B5?BL�BXB^5BffB�B�=B�Bt�Bl�BffB^5BYBXBVBR�BO�BN�BO�BP�BT�BXB`BBo�Bv�B}�B�{B��B��B��B��B�hB�PB�=B�1B�Bx�Bm�Bo�Bu�Bu�Bk�BiyBiyBaHBT�BI�BA�B?}B>wB=qB>wB>wBE�BG�BH�BL�BK�BL�BQ�BT�BZBW
BW
BW
BVBXBW
BT�BYB[#BZBZBaHBs�B�B�JB�\B��B��B�B�?B�?B�FB�jB�}B�}B��BBɺB��B��B��B��B��B��BȴBĜB��BĜBĜBÖB��B�wB�^B�dB�dB�dB�qB��BBǮB��B��B�B�B�B�B�)B�5B�;B�BB�HB�NB�ZB�fB�B��B��B	B	B	B	B	B	B	B	+B	%B		7B	B	B	B	B	B	%B	
=B	VB	\B	oB	�B	)�B	(�B	&�B	 �B	%�B	+B	%�B	#�B	�B	(�B	,B	5?B	E�B	R�B	T�B	VB	XB	ZB	[#B	]/B	^5B	_;B	aHB	aHB	dZB	gmB	iyB	k�B	l�B	m�B	p�B	r�B	u�B	x�B	|�B	~�B	�B	�B	�1B	�7B	�DB	�VB	�hB	�oB	�oB	�uB	�uB	�{B	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�-B	�3B	�9B	�?B	�?B	�FB	�FB	�LB	�XB	�jB	�jB	�wB	�}B	��B	��B	��B	��B	��B	B	ĜB	ŢB	ĜB	B	B	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�)B	�)B	�/B	�/B	�5B	�5B	�;B	�;B	�BB	�BB	�HB	�NB	�NB	�NB	�NB	�NB	�HB	�BB	�BB	�BB	�;B	�5B	�/B	�#B	�)B	�/B	�;B	�BB	�BB	�HB	�TB	�ZB	�ZB	�`B	�`B	�ZB	�`B	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�fB	�mB	�sB	�mB	�`B	�yB	�B	�B	�B	�B	�B	��B	�B	��B
bB
�B
�B
'�B
/B
5?B
;dB
A�B
F�B
L�B
S�B
YB
\)B
`BB
e`B
jB
m�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BhlBitBirBitBirBirBirBitBitBirBitBitBirBirBirBitBitBirBirBitBitBitBirBirBirBirBirBirBirBjyBitBirBitBf\BipB�B��BBABEB�B�uB�5BQB"�B�B�B!�B6>BH�BT�BO�BM�BN�BhlB{�B�lB��B��B��B��B�$B�B�+B��B�B��B�B��B��B�ZB|�B�iB��B��B�AB�%Bz�BbFBX	BK�B3-B�BOB��B�B�,B��Bz�Bm�BgdBW BL�B\ B�B�Bw�BisBhgBJ�B
��B
��B
�TB
'�B	��B	�KB	�0B	��B	ŚB	ȫB	B	�oB	�JB	ĔB	�$B	��B	�sB	�JB	�8B	�B	��B	�}B	�9B	�	B	}�B	u�B	o�B	gcB	XB	N�B	I�B	;\B	1B	�B	RB��B�,BɲB�fB�B��B��B��B�EB��Bw�Bk{B^,BP�B0B�B�B�B�B �B!�B�B�B58BL�BXB^+Bf]B�B�4B��Bt�Bl~Bf^B^*BYBXBU�BR�BO�BN�BO�BP�BT�BXB`7Bo�Bv�B}�B�qB��B��B��B��B�_B�EB�2B�&B��Bx�Bm�Bo�Bu�Bu�Bk|BimBinBa=BT�BI�BA�B?uB>mB=fB>lB>lBE�BG�BH�BL�BK�BL�BQ�BT�BZBW BV�BW BU�BXBV�BT�BY
B[BZBZBa?Bs�B�B�>B�PB�wB��B�B�4B�4B�:B�]B�pB�qB�yBBɯBʶB��B��B��B˽BʶBȩBďB�wBēBĐBÊB�~B�mB�PB�XB�XB�YB�eB�uBBǣB��B��B��B�B�B�B�B�*B�0B�6B�=B�DB�KB�ZB�B��B��B	 �B	B	B	�B	B	B	B	 B	B		*B	B	B	B	B	B	B	
3B	LB	OB	`B	�B	)�B	(�B	&�B	 �B	%�B	*�B	%�B	#�B	�B	(�B	+�B	53B	E�B	R�B	T�B	U�B	XB	ZB	[B	]$B	^(B	_/B	a;B	a>B	dNB	gaB	ilB	kyB	l�B	m�B	p�B	r�B	u�B	x�B	|�B	~�B	�B	�B	�&B	�*B	�:B	�JB	�\B	�dB	�dB	�kB	�jB	�pB	�gB	�hB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�!B	�"B	�(B	�-B	�5B	�1B	�;B	�9B	�BB	�LB	�_B	�_B	�iB	�mB	�wB	�uB	�wB	�wB	�B	B	ďB	œB	đB	B	B	ēB	ŕB	ǠB	ȩB	ʳB	˽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�$B	�*B	�'B	�/B	�0B	�4B	�4B	�<B	�BB	�@B	�CB	�BB	�BB	�<B	�7B	�7B	�6B	�/B	�)B	�#B	�B	�B	�%B	�/B	�4B	�5B	�<B	�EB	�LB	�OB	�TB	�TB	�OB	�TB	�[B	�YB	�`B	�`B	�aB	�`B	�aB	�[B	�aB	�fB	�aB	�UB	�mB	�sB	�B	�B	�B	�B	��B	�pB	��B
VB
zB
�B
'�B
/B
53B
;YB
A}B
F�B
L�B
S�B
YB
\B
`5B
eUB
jrB
m�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.09 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332472019040813324720190408133247  AO  ARCAADJP                                                                    20181121125830    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125830  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125830  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133247  IP                  G�O�G�O�G�O�                