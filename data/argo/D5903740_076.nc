CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:44Z AOML 3.0 creation; 2016-06-01T00:08:18Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230844  20160531170818  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               LA   AO  4055_7112_076                   2C  D   APEX                            5374                            041511                          846 @�퍜�1   @��0[ @:|�hs�d\(�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    LA   A   A   @�ff@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dx� D�	�D�I�D�s3D��fD�  D�FfD��3D�� D���D�@ D�y�D��3D��D�<�Dڠ D��3D�  D�C3D�y�D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�Az�A"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�Dx�D�]D�O]D�x�D��)D��D�L)D���D���D��D�E�D�]D���D�]D�B�Dڥ�D���D��D�H�D�]D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A� �A��A�oA��A��A��A��A��A� �A�+A�1'A�/A�1'A�1'A�33A�33A�5?A�5?A�7LA�9XA�9XA�;dA�;dA�=qA�=qA�=qA�33A�1'A�
=A�bA��;A�z�A�$�A�z�A���A�K�A��PA���A�VA�A��jA���A�ffA�5?A�A���A��A�bNA��;A���A�t�A���A��#A���A��7A�A�ZA��yA�ƨA�z�A���A��A��FA�"�A��PA��`A��A��^A�|�A��jA�&�A��\A��A���A�E�A~^5A{?}Az�RAzVAy�
Ax��AwXAv1As|�Ap^5AoXAnM�AlQ�Aj�AjE�Ai�-Ah�Ag�TAf��Aex�Ad$�Aa��AaXA`jA]�^A\��A\A[��A[�AY��AX1'AW��AV�AU�#AUhsAT��ARĜAQ�PAP��AO�FAOt�AOG�AN  AL�HALȴAL��ALAJ�AI&�AHVAF�AES�AD��AC��ACC�AB(�A@��A>ffA=l�A<�HA<�HA<��A;��A;O�A;oA:�A9�TA9t�A9p�A9\)A9G�A8��A8��A8$�A7\)A6{A3�PA0��A/+A.=qA-�A+�
A+�A+hsA+C�A++A+�A*�A*�jA*bNA*{A)�A)�-A)/A(��A(I�A'��A'C�A&��A&�9A%�A%;dA#t�A"v�A"jA"M�A!�A!�wA!;dA�A(�A�^A��A��A�#A&�AZA �A�A�\A��A��A�`AQ�A1A+A�^A?}A"�A��A
��A
A	p�A�A(�A��AK�A�A�/Av�A�A�A+Av�A�^A��A�A ��A bNA  �@��m@��F@��H@���@��`@�;d@���@���@���@�hs@�O�@���@�\)@�@�V@�D@�ƨ@�h@�/@�;d@�G�@�  @�$�@�z�@�dZ@އ+@ݡ�@ܴ9@�  @۾w@ۮ@�|�@��y@�ff@�-@ٙ�@�I�@֗�@�n�@�E�@��/@ӥ�@�n�@У�@υ@�@˅@ʇ+@�O�@�Z@ǅ@���@�5?@�%@�V@�I�@�5?@���@�/@��w@�n�@��^@�hs@�7L@�/@�/@��@�&�@�V@���@�n�@��T@�&�@�1@��w@��@�dZ@�C�@�+@�
=@���@��@��@�5?@�`B@���@��@�b@���@��!@�-@���@�t�@���@�^5@���@�`B@�&�@���@�t�@���@���@��@�?}@��;@���@��@�O�@��@�ƨ@�33@�ȴ@�n�@�=q@��@�@�x�@�&�@��D@���@���@�"�@�
=@���@�=q@���@���@�Z@��@�  @���@��;@��F@�S�@�o@�ȴ@��!@�n�@�X@�r�@�9X@�b@�1@�  @�  @���@��;@�ƨ@�l�@�C�@�33@�o@�ȴ@�v�@�5?@���@�hs@��@�%@�%@��@���@�r�@�A�@�1@�S�@��@��R@�@�hs@�O�@�V@�Ĝ@��@�Z@�I�@�(�@�1@��;@��@�|�@�dZ@�"�@��@��!@�~�@�^5@�E�@�{@�O�@�V@��u@��u@��u@�(�@��;@�ƨ@�ƨ@�ƨ@���@��@���@�K�@�
=@��!@���@��\@�~�@�v�@�n�@�ff@�^5@�^5@�5?@���@���@���@�x�@�O�@�V@�9X@K�@~�R@~��@~��@~V@}��@}�-@}?}@|��@{C�@zM�@y��@w��@w|�@w
=@v�@vȴ@vȴ@v��@v{@up�@t��@t�@sƨ@s��@st�@sC�@so@r�@r�H@r�H@r�H@r�H@r�H@r�H@r��@r��@r�\@rM�@r�@rJ@q�@n��@c"�@^�y@V$�@P��@L(�@Fȴ@@�`@;"�@4�@17L@*�@$��@ A�@Z@�@�@Q�@��@	X@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A� �A��A�oA��A��A��A��A��A� �A�+A�1'A�/A�1'A�1'A�33A�33A�5?A�5?A�7LA�9XA�9XA�;dA�;dA�=qA�=qA�=qA�33A�1'A�
=A�bA��;A�z�A�$�A�z�A���A�K�A��PA���A�VA�A��jA���A�ffA�5?A�A���A��A�bNA��;A���A�t�A���A��#A���A��7A�A�ZA��yA�ƨA�z�A���A��A��FA�"�A��PA��`A��A��^A�|�A��jA�&�A��\A��A���A�E�A~^5A{?}Az�RAzVAy�
Ax��AwXAv1As|�Ap^5AoXAnM�AlQ�Aj�AjE�Ai�-Ah�Ag�TAf��Aex�Ad$�Aa��AaXA`jA]�^A\��A\A[��A[�AY��AX1'AW��AV�AU�#AUhsAT��ARĜAQ�PAP��AO�FAOt�AOG�AN  AL�HALȴAL��ALAJ�AI&�AHVAF�AES�AD��AC��ACC�AB(�A@��A>ffA=l�A<�HA<�HA<��A;��A;O�A;oA:�A9�TA9t�A9p�A9\)A9G�A8��A8��A8$�A7\)A6{A3�PA0��A/+A.=qA-�A+�
A+�A+hsA+C�A++A+�A*�A*�jA*bNA*{A)�A)�-A)/A(��A(I�A'��A'C�A&��A&�9A%�A%;dA#t�A"v�A"jA"M�A!�A!�wA!;dA�A(�A�^A��A��A�#A&�AZA �A�A�\A��A��A�`AQ�A1A+A�^A?}A"�A��A
��A
A	p�A�A(�A��AK�A�A�/Av�A�A�A+Av�A�^A��A�A ��A bNA  �@��m@��F@��H@���@��`@�;d@���@���@���@�hs@�O�@���@�\)@�@�V@�D@�ƨ@�h@�/@�;d@�G�@�  @�$�@�z�@�dZ@އ+@ݡ�@ܴ9@�  @۾w@ۮ@�|�@��y@�ff@�-@ٙ�@�I�@֗�@�n�@�E�@��/@ӥ�@�n�@У�@υ@�@˅@ʇ+@�O�@�Z@ǅ@���@�5?@�%@�V@�I�@�5?@���@�/@��w@�n�@��^@�hs@�7L@�/@�/@��@�&�@�V@���@�n�@��T@�&�@�1@��w@��@�dZ@�C�@�+@�
=@���@��@��@�5?@�`B@���@��@�b@���@��!@�-@���@�t�@���@�^5@���@�`B@�&�@���@�t�@���@���@��@�?}@��;@���@��@�O�@��@�ƨ@�33@�ȴ@�n�@�=q@��@�@�x�@�&�@��D@���@���@�"�@�
=@���@�=q@���@���@�Z@��@�  @���@��;@��F@�S�@�o@�ȴ@��!@�n�@�X@�r�@�9X@�b@�1@�  @�  @���@��;@�ƨ@�l�@�C�@�33@�o@�ȴ@�v�@�5?@���@�hs@��@�%@�%@��@���@�r�@�A�@�1@�S�@��@��R@�@�hs@�O�@�V@�Ĝ@��@�Z@�I�@�(�@�1@��;@��@�|�@�dZ@�"�@��@��!@�~�@�^5@�E�@�{@�O�@�V@��u@��u@��u@�(�@��;@�ƨ@�ƨ@�ƨ@���@��@���@�K�@�
=@��!@���@��\@�~�@�v�@�n�@�ff@�^5@�^5@�5?@���@���@���@�x�@�O�@�V@�9X@K�@~�R@~��@~��@~V@}��@}�-@}?}@|��@{C�@zM�@y��@w��@w|�@w
=@v�@vȴ@vȴ@v��@v{@up�@t��@t�@sƨ@s��@st�@sC�@so@r�@r�H@r�H@r�H@r�H@r�H@r�H@r��@r��@r�\@rM�@r�@rJ@q�@n��@c"�@^�y@V$�@P��@L(�@Fȴ@@�`@;"�@4�@17L@*�@$��@ A�@Z@�@�@Q�@��@	X@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�\B�Bz�BgmB@�B�B�B�HB�mB�yB�yB�yB�sB�yB�sB�yB�ZBŢB�9B�Bx�Be`BS�B7LB�B��B�sB�)BŢB�B�'B�RB�^B�B��B�\B[#B5?B�B
��B
ĜB
��B
�oB
|�B
bNB
<jB
%�B
 �B
�B
�B
\B
B	��B	�fB	��B	��B	��B	�9B	��B	��B	��B	��B	��B	�oB	�7B	{�B	k�B	m�B	� B	y�B	t�B	q�B	p�B	m�B	e`B	`BB	]/B	ZB	VB	S�B	O�B	H�B	D�B	@�B	>wB	<jB	:^B	6FB	33B	2-B	1'B	.B	)�B	%�B	"�B	�B	�B	{B	bB	PB	+B	B��B��B�B�B�B�B�B�B�sB�fB�fB�fB�mB�fB�`B�TB�BB�#B�B��BB�jB�XB�RB�-B�3B�3B�3B�3B�3B�3B�-B�-B�-B�'B�!B�B�B��B��B��B��B��B��B��B�\B�JB�JB�DB�=B�7B�%B� B{�Bz�Bx�Br�Bl�BgmB_;BZBYBW
BVBT�BR�BP�BO�BK�BI�BH�BG�BD�BA�B?}B=qB<jB:^B:^B9XB8RB7LB6FB5?B49B33B1'B/B-B+B+B)�B)�B)�B(�B'�B&�B$�B$�B#�B#�B#�B"�B"�B!�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B#�B#�B#�B"�B"�B'�B+B-B,B)�B)�B,B-B.B.B/B/B0!B0!B33B49B6FB7LB9XB<jB<jB<jB<jB=qB=qB=qB=qB=qB=qB?}BA�BB�BC�BD�BE�BH�BI�BL�BR�BT�BVBXBYBZB[#B_;B`BBaHBbNBe`BiyBr�B� B�7B�hB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�FB�dB�wB��B��B��B��BBĜBƨBǮBȴBȴB��B��B�
B�#B�#B�/B�/B�/B�5B�;B�BB�HB�HB�NB�TB�`B�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B��B	B	B		7B	DB	PB	PB	PB	VB	\B	bB	uB	�B	�B	�B	�B	�B	!�B	"�B	#�B	$�B	,B	/B	33B	33B	49B	:^B	=qB	>wB	>wB	>wB	?}B	@�B	?}B	A�B	C�B	G�B	H�B	H�B	I�B	I�B	J�B	J�B	J�B	J�B	L�B	M�B	P�B	Q�B	Q�B	R�B	S�B	ZB	`BB	bNB	cTB	cTB	dZB	gmB	gmB	jB	l�B	s�B	x�B	z�B	}�B	~�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�JB	�VB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�/B	��B
B
\B
�B
�B
$�B
2-B
8RB
A�B
H�B
N�B
T�B
\)B
bNB
e`B
jB
n�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�oB�PB�Bz�BgaB@sB�B�B�7B�]B�lB�kB�jB�dB�hB�dB�gB�KBŒB�,B��Bx�BeNBS�B7:BuB��B�dB�BőB�B�B�?B�MB�B��B�JB[B5.BpB
��B
ĎB
��B
�`B
|�B
bAB
<_B
%�B
 �B
�B
�B
SB
B	��B	�\B	��B	ʵB	��B	�.B	��B	��B	��B	��B	��B	�fB	�2B	{�B	k�B	m�B	�B	y�B	t�B	q�B	p�B	m�B	eZB	`<B	]*B	ZB	U�B	S�B	O�B	H�B	D�B	@B	>rB	<dB	:YB	6?B	3,B	2(B	1!B	.B	)�B	%�B	"�B	�B	�B	xB	`B	LB	&B	
B��B��B�B�B�B�B�B�B�rB�bB�cB�dB�jB�eB�\B�SB�@B� B�B��BB�hB�WB�OB�.B�2B�3B�1B�4B�1B�3B�,B�-B�+B�&B�#B�B�B��B��B��B��B��B��B��B�_B�KB�MB�HB�?B�9B�&B�B{�Bz�Bx�Br�Bl�BgnB_=BZBYBWBVBT�BR�BP�BO�BK�BI�BH�BG�BD�BA�B?�B=WB<lB:`B:`B9AB8VB7PB6GB5BB4>B36B1*B/B-B+B+B*B*B* B(�B'�B&�B$�B$�B#�B#�B#�B"�B"�B!�B!�B �B �B�B�B�B�B�B�B�BrB�B�B�B�B�B�B�B�B�B�B�B|B�B�B�B�B�B�B�B�B�B�B�B �B!�B"�B#�B#�B#�B"�B"�B'�B+B-B,B)�B)�B,B-B.B.B/B/B0%B0"B35B4:B6FB7JB9YB<jB<iB<kB<jB=sB=rB=sB=uB=sB=qB?|BA�BB�BC�BD�BE�BH�BI�BL�BR�BT�BVBXBYBZB[#B_:B`@BaGBbLBe^BiwBr�B�B�7B�gB�kB�|B��B��B��B��B��B��B��B��B��B��B�B�B�B�&B�AB�]B�oB�}B��B��B��BBėBƤBǥBȮBȮB��B��B�B�B�B�&B�'B�(B�.B�5B�:B�@B�@B�HB�NB�WB�cB�pB�vB�B�B�B�B��B��B��B��B��B��B��B��B	B	B		-B	:B	HB	HB	FB	JB	TB	ZB	iB	|B	�B	�B	�B	�B	!�B	"�B	#�B	$�B	+�B	/B	3)B	3)B	40B	:RB	=eB	>hB	>kB	>hB	?sB	@wB	?sB	A~B	C�B	G�B	H�B	H�B	I�B	I�B	J�B	J�B	J�B	J�B	L�B	M�B	P�B	Q�B	Q�B	R�B	S�B	ZB	`5B	b@B	cFB	cHB	dNB	g`B	g^B	jqB	l|B	s�B	x�B	z�B	}�B	~�B	��B	�B	�B	�B	�B	��B	�B	�B	�#B	�/B	�6B	�;B	�HB	�fB	�mB	�nB	�oB	�oB	�tB	�oB	�rB	�uB	�wB	��B	��B	��B	��B	��B	��B	��B	�B	��B
B
HB
lB
�B
$�B
2B
8?B
AwB
H�B
N�B
T�B
\B
b9B
eLB
jkB
n�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.18 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708182016053117081820160531170818  AO  ARCAADJP                                                                    20140721230844    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230844  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230844  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170818  IP                  G�O�G�O�G�O�                