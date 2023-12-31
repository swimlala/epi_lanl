CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-09-11T19:16:38Z AOML 3.0 creation; 2016-08-07T21:36:38Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150911191638  20160807143639  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               GA   AO  5286_8897_071                   2C  D   APEX                            6531                            072314                          846 @�n�sKy)1   @�n�`�m@41���l��c ���F1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    GA   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�33B   B  BffB  B   B(  B0  B8  B@  BH  BP  BY33B_33Bg��Bp  Bx  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy� D�fD�I�D�s3D��3D�  D�FfD�� D�� D� D�9�D��fD��3D�fD�C3D�vfD� D� D�C3D�ffD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�A�HA"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�A�p�A�=qA��B �RB�RB�B�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBY�B_�BhQ�Bp�RBx�RB�\)B�\)B�\)B�\)B��\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*.C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.CpG�Cr.Ct.Cv.Cx.Cz.C|.C~.C�
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
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dtk�Dy��D�)D�O]D�x�D���D��D�L)D���D���D��D�?]D��)D���D�)D�H�D�|)D��D��D�H�D�l)D��]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�l�A�jA�hsA�hsA�hsA�jA�n�A�n�A�n�A�p�A�r�A�l�A�r�A�A�A�G�A�1'AݼjA���A���A��A�K�A�M�A��A���A�"�Aա�A�dZA��A΋DA���A�
=AˁA�M�A���A���A�ffA�;dAħ�Aú^A�A��jA��^A�|�A�G�A�E�A���A�;dA�
=A���A�+A��A��PA��uA�ƨA�JA��;A�|�A���A���A�
=A��#A��A�%A��A���A��A�JA�~�A���A�ZA�C�A��^A��!A�9XA���A���A���A��
A�1A���A���A���A�hsA�1'A�v�A��A�  A�A�A��A��+A�~�A�\)A���A�bNA�;dA�ĜA��-A�(�A���A��A��A���A�$�A�(�A��A�jA�bA�A��+A�5?A��A���A�l�A�G�A���A��uA�E�A~JA{&�Ay�hAx�jAt�`Ao%Al��Ai�^Ad�jA_��A[A[G�AZ9XAV�+AUAT��AS��AQ�
APffAO�AM�TAL �AI7LADM�AB��ABA@�A?�A>�A>5?A=C�A;�TA:1A9p�A8I�A6ȴA6VA5p�A4^5A2��A2�A1K�A0-A.ĜA,�A*�/A(��A($�A'�-A'
=A&VA%�PA%&�A#�PA"5?A!�A JA��A��A1AdZA �A�AĜA�FA�A��A1'A�PA��AE�A�wA�A�A�DA9XA�A�A
��A
Q�A	S�A	oA�uA�wA33A�AA�+A �/A ^5@�C�@��@��;@�j@�Q�@��m@�t�@�@��R@�E�@���@���@���@���@�I�@�|�@���@�r�@�;d@�\@�@��@�+@�^5@�1@�R@���@�dZ@�33@�;d@�"�@◍@߮@��#@�E�@��y@�x�@�Q�@�M�@؃@թ�@�Z@�?}@�V@��T@ՙ�@�j@�l�@�ff@с@�Ĝ@�dZ@ͩ�@��@�+@�"�@�$�@�(�@ǶF@ɉ7@��@��@�@�;d@�ff@� �@؋D@�(�@��@�j@��;@� �@���@���@�&�@ӍP@��@��@�"�@��@��`@��@��H@���@�1'@̴9@�&�@�ƨ@�|�@˝�@��;@���@��#@�E�@̣�@̃@�
=@��H@�$�@�`B@�Ĝ@�J@Ə\@ċD@��h@�S�@��-@���@�~�@�n�@�@���@�r�@�\)@�-@���@���@��@�Z@�A�@��w@�Ĝ@���@�/@��D@��;@��7@�r�@�ƨ@�ȴ@��!@��@��@��7@�/@�  @��@�K�@��@�ƨ@�A�@�|�@��@�+@��P@��@�
=@�&�@��@���@�A�@��D@���@���@��/@�&�@�&�@�%@���@�S�@�~�@��R@��@�
=@�S�@�S�@�C�@��R@�J@��@�x�@�G�@���@��/@��u@�A�@�b@��w@�
=@��\@���@���@�^5@�{@��@��@�G�@���@�1'@��P@���@��@�v�@�$�@��@��T@�X@�hs@��h@��7@�x�@�V@��P@�
=@��@��+@�^5@��@���@��#@�`B@���@���@���@�1'@�b@�p�@�%@��@�dZ@��+@��R@���@���@�l�@��@���@��@�ff@���@�@��y@���@���@�5?@�?}@��@���@��D@�1@�1@���@��+@�E�@�J@�x�@���@� �@�S�@��H@��#@��-@���@�&�@�A�@���@�9X@��@���@�33@���@�ff@��+@��@���@�`B@��h@��#@��#@��^@��`@��
@�+@��@��@�dZ@�Q�@���@�Ĝ@�j@�1'@�C�@���@v�R@nv�@c�@[�
@Pb@IG�@BM�@97L@333@-p�@'�;@#�@�w@��@�h@�u@�-@	�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�n�A�l�A�jA�hsA�hsA�hsA�jA�n�A�n�A�n�A�p�A�r�A�l�A�r�A�A�A�G�A�1'AݼjA���A���A��A�K�A�M�A��A���A�"�Aա�A�dZA��A΋DA���A�
=AˁA�M�A���A���A�ffA�;dAħ�Aú^A�A��jA��^A�|�A�G�A�E�A���A�;dA�
=A���A�+A��A��PA��uA�ƨA�JA��;A�|�A���A���A�
=A��#A��A�%A��A���A��A�JA�~�A���A�ZA�C�A��^A��!A�9XA���A���A���A��
A�1A���A���A���A�hsA�1'A�v�A��A�  A�A�A��A��+A�~�A�\)A���A�bNA�;dA�ĜA��-A�(�A���A��A��A���A�$�A�(�A��A�jA�bA�A��+A�5?A��A���A�l�A�G�A���A��uA�E�A~JA{&�Ay�hAx�jAt�`Ao%Al��Ai�^Ad�jA_��A[A[G�AZ9XAV�+AUAT��AS��AQ�
APffAO�AM�TAL �AI7LADM�AB��ABA@�A?�A>�A>5?A=C�A;�TA:1A9p�A8I�A6ȴA6VA5p�A4^5A2��A2�A1K�A0-A.ĜA,�A*�/A(��A($�A'�-A'
=A&VA%�PA%&�A#�PA"5?A!�A JA��A��A1AdZA �A�AĜA�FA�A��A1'A�PA��AE�A�wA�A�A�DA9XA�A�A
��A
Q�A	S�A	oA�uA�wA33A�AA�+A �/A ^5@�C�@��@��;@�j@�Q�@��m@�t�@�@��R@�E�@���@���@���@���@�I�@�|�@���@�r�@�;d@�\@�@��@�+@�^5@�1@�R@���@�dZ@�33@�;d@�"�@◍@߮@��#@�E�@��y@�x�@�Q�@�M�@؃@թ�@�Z@�?}@�V@��T@ՙ�@�j@�l�@�ff@с@�Ĝ@�dZ@ͩ�@��@�+@�"�@�$�@�(�@ǶF@ɉ7@��@��@�@�;d@�ff@� �@؋D@�(�@��@�j@��;@� �@���@���@�&�@ӍP@��@��@�"�@��@��`@��@��H@���@�1'@̴9@�&�@�ƨ@�|�@˝�@��;@���@��#@�E�@̣�@̃@�
=@��H@�$�@�`B@�Ĝ@�J@Ə\@ċD@��h@�S�@��-@���@�~�@�n�@�@���@�r�@�\)@�-@���@���@��@�Z@�A�@��w@�Ĝ@���@�/@��D@��;@��7@�r�@�ƨ@�ȴ@��!@��@��@��7@�/@�  @��@�K�@��@�ƨ@�A�@�|�@��@�+@��P@��@�
=@�&�@��@���@�A�@��D@���@���@��/@�&�@�&�@�%@���@�S�@�~�@��R@��@�
=@�S�@�S�@�C�@��R@�J@��@�x�@�G�@���@��/@��u@�A�@�b@��w@�
=@��\@���@���@�^5@�{@��@��@�G�@���@�1'@��P@���@��@�v�@�$�@��@��T@�X@�hs@��h@��7@�x�@�V@��P@�
=@��@��+@�^5@��@���@��#@�`B@���@���@���@�1'@�b@�p�@�%@��@�dZ@��+@��R@���@���@�l�@��@���@��@�ff@���@�@��y@���@���@�5?@�?}@��@���@��D@�1@�1@���@��+@�E�@�J@�x�@���@� �@�S�@��H@��#@��-@���@�&�@�A�@���@�9X@��@���@�33@���@�ff@��+@��@���@�`B@��h@��#@��#@��^@��`@��
@�+@��@��@�dZ@�Q�@���@�ĜG�O�@�1'@�C�@���@v�R@nv�@c�@[�
@Pb@IG�@BM�@97L@333@-p�@'�;@#�@�w@��@�h@�u@�-@	�#11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
VB
JB
�B
\)B
�LB
�fBJB?}B�B�=B�!B�B�7B�+B�HB�ZB�`B�BB��BBoB�B�B0!B9XBI�BJ�BW
B��B�B��BB%B%BBBB��BBB��B{B$�B)�B!�B(�B�BbB+B��B��B��B��B��B�B�fB�B��B��B��BɺBÖB��B�^B�9B��B��B��B��B�VB}�Bt�BffBP�B@�B+B�B�B�B	7B�B��B�3B��B~�BL�B5?B9XB�B
�B
��B
�9B
��B
�B
}�B
q�B
ZB
49B
�B
�B
/B
33B
�B

=B	��B	�yB	�BB	ŢB	��B	�=B	iyB	=qB	�B	
=B	%B��B��B��B�B�B�`B�5B�B��BȴB�LB��B��B��B��B��B��B��B��B�{B�{B�hB�VB�DB�1B�B�B~�B}�B}�B}�B{�Bu�Bs�Bx�B�B�%B�7B�7B�7B�1B�B�B�B{�B�%B�DB�JB�bB�hB�hB�bB�oB�uB�uB�oB�hB�uB�uB�oB�hB�%B}�B~�B�7B{�B`BBcTBaHB`BB\)B[#B[#B_;B`BB`BB`BB`BBbNBaHB`BBt�By�Bz�Bz�Bz�B{�B�1B�JB�7B�DB�VB�VB�oB�uB�bB�PB�PB�DB�B�B�B{�Bz�B{�Bw�Bz�B}�B~�B~�B|�By�B~�B�%B�B�B{�B� B� B�B�PB��B��B��B��B��B��B��B�B��B�B�B�!B�?B�XB��BƨB�B�B�B�B�B	,B	?}B	ZB	[#B	cTB	bNB	aHB	cTB	cTB	cTB	bNB	\)B	YB	^5B	bNB	_;B	\)B	YB	T�B	P�B	S�B	[#B	`BB	]/B	\)B	_;B	cTB	bNB	aHB	gmB	v�B	y�B	u�B	x�B	v�B	t�B	s�B	iyB	p�B	iyB	`BB	YB	S�B	O�B	L�B	M�B	M�B	M�B	L�B	J�B	I�B	H�B	H�B	H�B	I�B	J�B	M�B	XB	cTB	ffB	gmB	gmB	_;B	\)B	ZB	XB	XB	ZB	]/B	^5B	`BB	_;B	_;B	cTB	gmB	k�B	r�B	q�B	q�B	t�B	z�B	{�B	z�B	w�B	x�B	w�B	y�B	|�B	� B	� B	�B	�%B	�+B	�+B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�?B	�FB	�FB	�FB	�FB	�LB	�RB	�^B	�jB	�qB	�}B	B	B	B	ÖB	B	�}B	�wB	�}B	�}B	��B	B	ÖB	ÖB	ĜB	ƨB	ǮB	ǮB	ƨB	ŢB	ŢB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�/B	�/B	�B	�B	�B	�B	�#B	�)B	�BB	�mB	�B	�sB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B	��B	��B	��B	��B	��B
B
+B

=B

=B
\B
B
oB
�B
�B
�B
%�B
.B
5?B
:^B
A�B
F�B
N�B
T�B
YB
]/B
`BB
ffB
k�B
p�B
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B
[B
_B
]B
_B
]B
]B
]B
]B
]B
]B
]B
_B
]B
_B
RB
DB
�B
\!B
�DB
�\B>B?oB��B�.B�B�B�)B�B�9B�LB�RB�B �B��BB^B~B�B0B9IBI�BJ�BV�B�B�B��BBBBB B B��BBB��BkB$�B)�B!�B(�BsBWBB��B��B��B��B��B�B�WB��B��BʵB��BɪBÇB�sB�MB�)B��B��B��B��B�GB}�Bt�BfVBP�B@qB*�BsB�B�B	$B�B��B� B��B~�BL�B5.B9GB~B
�vB
��B
�+B
��B
�B
}�B
q�B
ZB
4,B
�B
�B
/B
3'B
�B

2B	��B	�pB	�;B	řB	��B	�7B	iqB	=lB	�B	
;B	#B��B��B��B�B�B�`B�7B�B��BȲB�MB��B��B��B��B��B��B��B��B�|B�}B�lB�VB�GB�3B�B�
B~�B}�B}�B}�B{�Bu�Bs�Bx�B�B�'B�:B�9B�9B�2B�B�B�B{�B�&B�EB�KB�cB�jB�jB�dB�qB�uB�tB�pB�kB�uB�uB�nB�gB�&B}�B~�B�6B{�B`CBcUBaJB`DB\+B[$B[$B_;B`BB`CB`DB`CBbMBaHB`BBt�By�Bz�Bz�Bz�B{�B�0B�GB�5B�DB�UB�SB�mB�sB�`B�PB�QB�EB�B�B�B{�Bz�B{�Bw�Bz�B}�B~�B~�B|�By�B~�B�"B�B�B{�B� B� B�B�LB��B��B��B��B��B��B��B�B��B�
B�B�B�;B�SB�zBƥB�B�B�B�B�B	+�B	?sB	ZB	[B	cHB	bBB	a=B	cJB	cIB	cGB	bCB	\B	Y
B	^*B	bCB	_1B	\B	YB	T�B	P�B	S�B	[B	`7B	]"B	\B	_1B	cIB	bBB	a@B	gbB	v�B	y�B	u�B	x�B	v�B	t�B	s�B	ilB	p�B	ilB	`5B	YB	S�B	O�B	L�B	M�B	M�B	M�B	L�B	J�B	I�B	H�B	H�B	H�B	I�B	J�B	M�B	XB	cJB	fZB	gaB	gbB	_.B	\B	ZB	XB	XB	ZB	]$B	^)B	`7B	_/B	_/B	cGB	g_B	kzB	r�B	q�B	q�B	t�B	z�B	{�B	z�B	w�B	x�B	w�B	y�B	|�B	�B	�B	�B	�B	�B	�B	�=B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�%B	�/B	�6B	�6B	�6B	�8B	�@B	�DB	�OB	�\B	�aB	�pB	B	B	B	ÄB	B	�nB	�iB	�jB	�mB	�{B	�B	ÆB	ÆB	ČB	ƗB	ǡB	ǡB	ƚB	ŒB	ŔB	ȢB	ɭB	ɫB	ɬB	˸B	˷B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	��B	�B	�B	�B	�2B	�]B	�kB	�cB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B	��B	��B	��B	��B	��B
 �B
B

-B

+G�O�B
B
\B
�B
�B
�B
%�B
.B
5.B
:KB
AxB
F�B
N�B
T�B
YB
]B
`0B
fRB
kqB
p�B
r�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.18 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436392016080714363920160807143639  AO  ARCAADJP                                                                    20150911191638    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150911191638  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20150911191638  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143639  IP                  G�O�G�O�G�O�                