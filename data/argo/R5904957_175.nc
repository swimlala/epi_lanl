CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:37Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140837  20181024140837  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��d�fٵ1   @��eu�~@5��^5?}�c���R1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�fC  C  C�fC  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CD  CF  CH  CJ  CK�fCN  CP  CR  CS�fCV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  D   D y�D ��D� D  D� D  D� D  Dy�D  D� D  D� D��Dy�D  D� D	fD	�fD
fD
� D  D� D  Dy�D  D� D  D� D  Dy�D  D� D  D� D  D� D��D� D  D�fDfD� D  D� D  D� D  D� D  Dy�D��D� D  D� D  D� D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7fD7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DC��DDy�DE  DE� DF  DF� DG  DG�fDH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DMy�DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dcy�Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Do� DpfDp�fDq  Dq� DrfDr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dw�fDy�)D�:�D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@\AG�A"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(Q�B0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.CzC.C.CzC.C.C.C.C G�C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@G�CBG�CD.CF.CH.CJ.CLzCN.CP.CR.CTzCV.CX.CZ.C\.C^.C`.Cb.Cd.CfG�Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
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
C�
=C�
=C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
C�
=C�
=C�
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
C�
=C�
=C�
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
C�#�C�#�C�
C�
C�#�C�
C�
C�
C�
=C�
=C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
=C�
=C�
D �D �DD��D�D��D�D��D�D�D�D��D�D��DD�D�D��D	�D	��D
�D
��D�D��D�D�D�D��D�D��D�D�D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D��D�D��D�D�DD��D�D��D�D��D�D��D�D��D�D��D �D �D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC�DDDD�DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DLDL��DM�DM�DN�DN��DO�DO�DP�DP��DQ�DQ��DR�DR��DS�DS��DTDT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ�D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh�Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw�Dw��Dy��D�@RD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� �A�(�A�-A�/A�33A�33A�7LA�A�A�?}A�C�A�E�A�=qA�1'A�7LA�5?A�"�A�1'A�"�A�oA��A��A��A���A���A���Aа!AЍPAЁAЃAЃAЁA�p�A�\)A�
=A��yA˝�A�~�A��`AǮA��
A��#A�ZA��FA���A�33A��A�C�A���A�G�A��A��A��A�{A�M�A��uA���A���A�A��A�-A���A�1'A���A��`A��
A�1'A�ZA�/A��`A�
=A��A��A�oA�%A�ZA��PA��A��A�{A��PA���A��7A�z�A�bA�"�A��jA�\)A���A��A�+A�n�A���A�I�A�dZA��PA��;A�1A�bNA��HA��jA�7LA�ZA�ȴA�1A���A�$�A�l�A�~�A��-A��HA�5?A��A�ĜA~I�A|5?Aw�Au�#Aq�Al�!AkVAi�
Ai��AhJAf�+Ac�^AbjAaƨAa&�A`��A^��A]�#A]�AY`BAWoAV�\AU�wAT��AS�7AQ��AP�HAPz�AN��AI+AH�AHv�AH^5AG�;AGAG�FAG��AG�hAG�AGS�AF��AF��AFv�AEƨAD�AB�`A@��A>��A=��A=ƨA<�A:  A8M�A6��A5oA3t�A2A�A1A01A/�7A/&�A-ƨA+VA*ffA)�#A(�A(A�A&��A%�hA$ffA#\)A!33AVA�+A�AVA��A��A�RAC�A��A��AƨA�Av�A33AA�A\)A�!AffA1A7LAffA
$�A	��A	hsA	/A�A��A�AS�A�wA?}A��A�DA ��@��m@�o@�@�~�@�/@���@��j@�@�C�@�!@���@�V@��@�7L@�O�@�x�@��`@���@���@�%@�^5@�V@�j@蛦@�D@�1'@���@�p�@�S�@�n�@�V@�5?@�-@�-@�Q�@���@ߍP@�@��@ܛ�@�r�@�Z@�Q�@��m@�=q@�7L@ؼj@�1@�
=@֧�@պ^@���@�hs@���@�Q�@�  @�l�@��y@ΰ!@·+@�-@�O�@�bN@�dZ@��@�ȴ@�n�@ɉ7@�z�@ǝ�@�
=@�5?@��#@�%@��
@��y@��@���@�(�@�dZ@���@��H@�ȴ@���@���@��R@��!@���@�~�@�n�@�=q@�J@�@���@���@��^@��-@��h@�G�@�?}@�&�@�Ĝ@�j@�1@���@�;d@��!@�^5@�5?@��@���@��@�9X@��R@��@��-@���@��u@�bN@�b@��@�ƨ@��@�K�@�;d@��@�v�@���@�X@�7L@���@���@��@�(�@���@���@�K�@�o@��@���@���@�M�@��T@�/@���@���@�z�@�Z@�1@���@�33@�^5@�J@�hs@���@��H@�A�@�b@��
@��@�-@��-@�&�@��9@�9X@�b@��@��@���@���@���@���@�S�@�
=@��y@��@���@���@�ff@�J@�@���@��@��@��T@�@��h@�G�@���@���@���@�bN@�A�@�  @��F@���@�v�@�{@��@�@���@��7@�&�@��/@��D@�(�@��@�1@��P@�@�v�@�J@��@��@���@���@���@��#@��T@��@��T@�x�@�O�@���@�Q�@��@���@�\)@�33@�+@��@�
=@��@��H@�ȴ@���@��-@���@���@��u@�I�@��@���@�S�@�K�@�33@��H@�v�@�$�@��@�{@�J@�@��@��u@�\)@���@���@���@�dZ@�K�@�;d@�o@�v�@�-@��#@�@��-@�x�@��@��@�Z@�1@���@�\)@�33@��@p"h@`4n111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A� �A�(�A�-A�/A�33A�33A�7LA�A�A�?}A�C�A�E�A�=qA�1'A�7LA�5?A�"�A�1'A�"�A�oA��A��A��A���A���A���Aа!AЍPAЁAЃAЃAЁA�p�A�\)A�
=A��yA˝�A�~�A��`AǮA��
A��#A�ZA��FA���A�33A��A�C�A���A�G�A��A��A��A�{A�M�A��uA���A���A�A��A�-A���A�1'A���A��`A��
A�1'A�ZA�/A��`A�
=A��A��A�oA�%A�ZA��PA��A��A�{A��PA���A��7A�z�A�bA�"�A��jA�\)A���A��A�+A�n�A���A�I�A�dZA��PA��;A�1A�bNA��HA��jA�7LA�ZA�ȴA�1A���A�$�A�l�A�~�A��-A��HA�5?A��A�ĜA~I�A|5?Aw�Au�#Aq�Al�!AkVAi�
Ai��AhJAf�+Ac�^AbjAaƨAa&�A`��A^��A]�#A]�AY`BAWoAV�\AU�wAT��AS�7AQ��AP�HAPz�AN��AI+AH�AHv�AH^5AG�;AGAG�FAG��AG�hAG�AGS�AF��AF��AFv�AEƨAD�AB�`A@��A>��A=��A=ƨA<�A:  A8M�A6��A5oA3t�A2A�A1A01A/�7A/&�A-ƨA+VA*ffA)�#A(�A(A�A&��A%�hA$ffA#\)A!33AVA�+A�AVA��A��A�RAC�A��A��AƨA�Av�A33AA�A\)A�!AffA1A7LAffA
$�A	��A	hsA	/A�A��A�AS�A�wA?}A��A�DA ��@��m@�o@�@�~�@�/@���@��j@�@�C�@�!@���@�V@��@�7L@�O�@�x�@��`@���@���@�%@�^5@�V@�j@蛦@�D@�1'@���@�p�@�S�@�n�@�V@�5?@�-@�-@�Q�@���@ߍP@�@��@ܛ�@�r�@�Z@�Q�@��m@�=q@�7L@ؼj@�1@�
=@֧�@պ^@���@�hs@���@�Q�@�  @�l�@��y@ΰ!@·+@�-@�O�@�bN@�dZ@��@�ȴ@�n�@ɉ7@�z�@ǝ�@�
=@�5?@��#@�%@��
@��y@��@���@�(�@�dZ@���@��H@�ȴ@���@���@��R@��!@���@�~�@�n�@�=q@�J@�@���@���@��^@��-@��h@�G�@�?}@�&�@�Ĝ@�j@�1@���@�;d@��!@�^5@�5?@��@���@��@�9X@��R@��@��-@���@��u@�bN@�b@��@�ƨ@��@�K�@�;d@��@�v�@���@�X@�7L@���@���@��@�(�@���@���@�K�@�o@��@���@���@�M�@��T@�/@���@���@�z�@�Z@�1@���@�33@�^5@�J@�hs@���@��H@�A�@�b@��
@��@�-@��-@�&�@��9@�9X@�b@��@��@���@���@���@���@�S�@�
=@��y@��@���@���@�ff@�J@�@���@��@��@��T@�@��h@�G�@���@���@���@�bN@�A�@�  @��F@���@�v�@�{@��@�@���@��7@�&�@��/@��D@�(�@��@�1@��P@�@�v�@�J@��@��@���@���@���@��#@��T@��@��T@�x�@�O�@���@�Q�@��@���@�\)@�33@�+@��@�
=@��@��H@�ȴ@���@��-@���@���@��u@�I�@��@���@�S�@�K�@�33@��H@�v�@�$�@��@�{@�J@�@��@��u@�\)@���@���@���@�dZ@�K�@�;d@�o@�v�@�-@��#@�@��-@�x�@��@��@�Z@�1@���@�\)@�33@��@p"h@`4n111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�dB�dB�dB�^B�^B�^B�dB�qB�jB�qB�}BBƨBĜBƨB��BǮB��B��B��B��B��B�#B�B�BB�fB�yB�B�B�B�B�B�B�B\B�B$�B$�B1'BL�BVBe`Bp�Bm�Bl�Bt�Bt�Bu�Bv�Bu�Bq�Bn�Bm�Bl�Bl�Br�B{�By�Bk�BhsBjBgmBdZBcTBbNB`BB\)BZBW
BH�B2-B"�B{BB�fB�/B�
BǮB�9B�B��B�hBw�BgmB[#BVBP�BI�B?}B1'B$�B�BuB+B  B
��B
�B
�NB
�5B
�5B
�
B
��B
�!B
��B
��B
��B
�{B
�=B
�B
u�B
l�B
bNB
H�B
2-B
"�B
1B	��B	�B	�^B	�B	��B	��B	��B	�bB	|�B	u�B	u�B	r�B	q�B	iyB	dZB	]/B	L�B	?}B	<jB	8RB	2-B	)�B	 �B	�B	�B	VB	B	B	  B	  B��B��B��B��B��B��B��B��B��B��B��B�B�yB�HB�/B�B�B��B��BƨB��B�wB�^B�RB�FB�?B�3B�'B�B��B��B��B��B��B��B��B�{B�oB�JB�+B�B� B� B� B}�B}�B�B�B�B� B|�Bx�Bv�Bq�Bo�Bp�Bp�Bo�Bn�BjBdZBdZBe`BdZBcTBbNB`BB^5B`BB^5B`BBk�B`BB[#BXBR�BK�BI�BH�BI�BM�BO�BQ�BiyBq�Bq�Bp�Bu�B{�B�=B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�'B�-B�-B�-B�-B�-B�?B�FB�LB�RB�^B�^B�dB��BÖBĜBŢBĜBĜBŢBŢBǮB��B��B��B��B�
B�B�B�5B�TB�mB�sB�B�B�B��B��B	  B	B	B	%B		7B	
=B	DB	JB	JB	PB	PB	PB	\B	bB	hB	oB	oB	oB	oB	oB	oB	oB	uB	uB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	&�B	33B	8RB	:^B	@�B	B�B	C�B	F�B	G�B	I�B	J�B	K�B	L�B	N�B	P�B	XB	\)B	^5B	_;B	`BB	aHB	e`B	gmB	hsB	jB	l�B	m�B	m�B	o�B	q�B	s�B	x�B	{�B	|�B	}�B	~�B	�B	�B	�B	�%B	�%B	�%B	�%B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�FB	�LB	�LB	�LB	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�qB	�}B	�}B	��B	ÖB	ƨB	ǮB	ƨB	ŢB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�;B	�HB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
�B
$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�dB�dB�dB�^B�^B�^B�dB�qB�jB�qB�}BBƨBĜBƨB��BǮB��B��B��B��B��B�#B�B�BB�fB�yB�B�B�B�B�B�B�B\B�B$�B$�B1'BL�BVBe`Bp�Bm�Bl�Bt�Bt�Bu�Bv�Bu�Bq�Bn�Bm�Bl�Bl�Br�B{�By�Bk�BhsBjBgmBdZBcTBbNB`BB\)BZBW
BH�B2-B"�B{BB�fB�/B�
BǮB�9B�B��B�hBw�BgmB[#BVBP�BI�B?}B1'B$�B�BuB+B  B
��B
�B
�NB
�5B
�5B
�
B
��B
�!B
��B
��B
��B
�{B
�=B
�B
u�B
l�B
bNB
H�B
2-B
"�B
1B	��B	�B	�^B	�B	��B	��B	��B	�bB	|�B	u�B	u�B	r�B	q�B	iyB	dZB	]/B	L�B	?}B	<jB	8RB	2-B	)�B	 �B	�B	�B	VB	B	B	  B	  B��B��B��B��B��B��B��B��B��B��B��B�B�yB�HB�/B�B�B��B��BƨB��B�wB�^B�RB�FB�?B�3B�'B�B��B��B��B��B��B��B��B�{B�oB�JB�+B�B� B� B� B}�B}�B�B�B�B� B|�Bx�Bv�Bq�Bo�Bp�Bp�Bo�Bn�BjBdZBdZBe`BdZBcTBbNB`BB^5B`BB^5B`BBk�B`BB[#BXBR�BK�BI�BH�BI�BM�BO�BQ�BiyBq�Bq�Bp�Bu�B{�B�=B�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�'B�-B�-B�-B�-B�-B�?B�FB�LB�RB�^B�^B�dB��BÖBĜBŢBĜBĜBŢBŢBǮB��B��B��B��B�
B�B�B�5B�TB�mB�sB�B�B�B��B��B	  B	B	B	%B		7B	
=B	DB	JB	JB	PB	PB	PB	\B	bB	hB	oB	oB	oB	oB	oB	oB	oB	uB	uB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	&�B	33B	8RB	:^B	@�B	B�B	C�B	F�B	G�B	I�B	J�B	K�B	L�B	N�B	P�B	XB	\)B	^5B	_;B	`BB	aHB	e`B	gmB	hsB	jB	l�B	m�B	m�B	o�B	q�B	s�B	x�B	{�B	|�B	}�B	~�B	�B	�B	�B	�%B	�%B	�%B	�%B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�VB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�?B	�FB	�LB	�LB	�LB	�FB	�LB	�RB	�XB	�^B	�^B	�dB	�qB	�}B	�}B	��B	ÖB	ƨB	ǮB	ƨB	ŢB	ŢB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�;B	�HB	�NB	�TB	�TB	�TB	�ZB	�`B	�fB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
�B
$�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140837                              AO  ARCAADJP                                                                    20181024140837    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140837  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140837  QCF$                G�O�G�O�G�O�0               