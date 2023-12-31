CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-23T19:17:35Z AOML 3.0 creation; 2016-08-07T21:36:40Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151023191735  20160807143640  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               OA   AO  5286_8897_079                   2C  D   APEX                            6531                            072314                          846 @�y+"�N�1   @�y+���@2�+�c&V�u1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    OA   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8ffB?33BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D� D�VfD�|�D��fD�3D�L�D���D�� D�fD�6fD�y�DǼ�D��D�S3Dڀ D�3D�fD�9�D�ffD��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�\)@�A�HA"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB)�B1�B9�B?�BH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
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
C�
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�RDy�D��D�\)D���D��)D��D�R�D��]D���D�)D�<)D�]D�D�"�D�X�Dڅ�D��D�)D�?]D�l)D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�|�A܍PA܋DAܙ�Aܗ�Aܕ�Aܕ�Aܙ�Aܙ�Aܕ�Aܟ�Aܡ�Aܡ�Aܡ�Aܝ�Aܝ�Aܝ�Aܣ�Aܥ�Aܥ�Aܥ�Aܩ�A�ffA��A�ƨA�K�A֬A�A�ZAң�A�9XAжFA��A���A�~�A�hsA��HA�Q�A��A�XA�G�A��RA��A�=qA��A��`A��A�^5A�n�A�\)A���A���A�C�A���A��HA��A��^A�XA�hsA��#A��jA���A�5?A���A�M�A��\A�%A�VA�"�A�JA�XA�z�A�ZA���A���A�dZA�^5A�ƨA��A��^A�Q�A���A�M�A���A�dZA�bNA��/A�l�A���A�{A���A�ffA��mA�ƨA�l�A� �A��wA���A��A�  A��A�A���A�O�A�A�A�(�A��A�+A�O�A}x�Av�\As7LAq?}ApĜApjAn�AlAkVAjv�AiƨAgO�A`$�A]�hA\5?A[&�AV��AQ��AMl�AK��AJ=qAF��AC�FAB��A@�HA?
=A<��A<  A:��A8A5O�A3�FA2=qA1��A1�A0�/A0��A01'A.ȴA.1A-x�A,�DA+oA)x�A)?}A)"�A(�uA'��A'A&(�A%S�A$�A"ĜA!��A�A��A�mA�
A/A�HA�FAjAE�A�#A|�A��A��A��A$�A  A��A��A�wA��A��A7LA�mA
=A�A
��A	�;A	`BA�yA��Ar�A�FA�\A��A��A^5A�wAVA $�@�o@��@��@��u@�b@��@�(�@�t�@���@���@��@��H@��T@�hs@��`@�1'@�;d@�=q@�@��T@��@��@�+@�\)@�+@�X@�z�@�$�@�@�\@�"�@�l�@��^@�X@�33@�@��;@��T@��@�O�@�r�@�\@�j@�/@�G�@���@�Z@߅@ޗ�@��@އ+@���@�o@��@�o@��@ޟ�@��@���@��T@�9X@��@��;@с@�"�@�/@Ͳ-@�/@�dZ@�?}@��/@��
@�
=@Ɨ�@�M�@�X@��/@���@��y@�(�@���@�  @�O�@�Z@�
=@�ȴ@��#@�|�@�dZ@�p�@�&�@��H@�%@�7L@��@��j@���@� �@���@���@���@�+@��^@�A�@�
=@��H@���@�ȴ@���@���@��@��@��j@��H@�J@���@���@�/@��u@�9X@��w@�l�@�t�@��D@�V@���@�bN@��P@�C�@�t�@�\)@��@��H@��#@�@��T@��u@�t�@�C�@�o@�+@���@�ȴ@�^5@��h@�`B@�V@���@�hs@�`B@�&�@�$�@�$�@�{@�@�  @��@��+@���@��T@���@���@��@��@��@�j@�j@��R@�A�@��@���@�C�@��;@�z�@��@�M�@��@���@��7@��7@��@�{@��@�{@�-@���@���@�/@�p�@���@��u@�  @�dZ@���@��y@�X@�hs@��;@��R@�n�@�M�@��#@��-@���@���@��@�Z@�I�@�b@���@��;@�t�@�C�@�+@���@�+@�ȴ@�ff@�5?@�$�@�$�@�`B@��`@��9@��@�z�@�(�@��@��w@���@���@���@��P@��P@�|�@�
=@�$�@��#@���@�x�@�`B@��@��`@��`@��@��`@��`@��j@���@��`@���@�dZ@��H@���@���@�=q@���@��@�p�@�hs@�G�@��@�z�@� �@��@��;@��;@��@�M�@�{@�@��@��^@�p�@�?}@���@���@��@��w@�dZ@�"�@��@��@�o@�
=@�@�ff@��@+@t�@k��@`Ĝ@Z~�@S��@J�!@D�/@=`B@7�@2�@,�@(A�@!�^@`B@�u@�!@ff@dZ@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�|�A܍PA܋DAܙ�Aܗ�Aܕ�Aܕ�Aܙ�Aܙ�Aܕ�Aܟ�Aܡ�Aܡ�Aܡ�Aܝ�Aܝ�Aܝ�Aܣ�Aܥ�Aܥ�Aܥ�Aܩ�A�ffA��A�ƨA�K�A֬A�A�ZAң�A�9XAжFA��A���A�~�A�hsA��HA�Q�A��A�XA�G�A��RA��A�=qA��A��`A��A�^5A�n�A�\)A���A���A�C�A���A��HA��A��^A�XA�hsA��#A��jA���A�5?A���A�M�A��\A�%A�VA�"�A�JA�XA�z�A�ZA���A���A�dZA�^5A�ƨA��A��^A�Q�A���A�M�A���A�dZA�bNA��/A�l�A���A�{A���A�ffA��mA�ƨA�l�A� �A��wA���A��A�  A��A�A���A�O�A�A�A�(�A��A�+A�O�A}x�Av�\As7LAq?}ApĜApjAn�AlAkVAjv�AiƨAgO�A`$�A]�hA\5?A[&�AV��AQ��AMl�AK��AJ=qAF��AC�FAB��A@�HA?
=A<��A<  A:��A8A5O�A3�FA2=qA1��A1�A0�/A0��A01'A.ȴA.1A-x�A,�DA+oA)x�A)?}A)"�A(�uA'��A'A&(�A%S�A$�A"ĜA!��A�A��A�mA�
A/A�HA�FAjAE�A�#A|�A��A��A��A$�A  A��A��A�wA��A��A7LA�mA
=A�A
��A	�;A	`BA�yA��Ar�A�FA�\A��A��A^5A�wAVA $�@�o@��@��@��u@�b@��@�(�@�t�@���@���@��@��H@��T@�hs@��`@�1'@�;d@�=q@�@��T@��@��@�+@�\)@�+@�X@�z�@�$�@�@�\@�"�@�l�@��^@�X@�33@�@��;@��T@��@�O�@�r�@�\@�j@�/@�G�@���@�Z@߅@ޗ�@��@އ+@���@�o@��@�o@��@ޟ�@��@���@��T@�9X@��@��;@с@�"�@�/@Ͳ-@�/@�dZ@�?}@��/@��
@�
=@Ɨ�@�M�@�X@��/@���@��y@�(�@���@�  @�O�@�Z@�
=@�ȴ@��#@�|�@�dZ@�p�@�&�@��H@�%@�7L@��@��j@���@� �@���@���@���@�+@��^@�A�@�
=@��H@���@�ȴ@���@���@��@��@��j@��H@�J@���@���@�/@��u@�9X@��w@�l�@�t�@��D@�V@���@�bN@��P@�C�@�t�@�\)@��@��H@��#@�@��T@��u@�t�@�C�@�o@�+@���@�ȴ@�^5@��h@�`B@�V@���@�hs@�`B@�&�@�$�@�$�@�{@�@�  @��@��+@���@��T@���@���@��@��@��@�j@�j@��R@�A�@��@���@�C�@��;@�z�@��@�M�@��@���@��7@��7@��@�{@��@�{@�-@���@���@�/@�p�@���@��u@�  @�dZ@���@��y@�X@�hs@��;@��R@�n�@�M�@��#@��-@���@���@��@�Z@�I�@�b@���@��;@�t�@�C�@�+@���@�+@�ȴ@�ff@�5?@�$�@�$�@�`B@��`@��9@��@�z�@�(�@��@��w@���@���@���@��P@��P@�|�@�
=@�$�@��#@���@�x�@�`B@��@��`@��`@��@��`@��`@��j@���@��`@���@�dZ@��H@���@���@�=q@���@��@�p�@�hs@�G�@��@�z�@� �@��@��;@��;@��@�M�@�{@�@��@��^@�p�@�?}@���@���@��@��w@�dZ@�"�@��@��@�o@�
=@�G�O�@��@+@t�@k��@`Ĝ@Z~�@S��@J�!@D�/@=`B@7�@2�@,�@(A�@!�^@`B@�u@�!@ff@dZ@l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�BPB�B�B+BE�BaHB}�B��B�FB��B�BB�B49BW
Bo�Bn�Bx�Bw�B�DB��B�3B�qBȴB��B�BB�TB�mB�mB�mB�B��BB��B��B��B��B��B��B��BBB��B�B�yB�sB�fB�B�BŢB��B�jB�B�bB�%Bv�BbNB]/BN�BF�B@�B/B�B�BJB��B�;BÖB�jB��Bw�B`BBXBB�B:^B?}B:^B�B
��B
��B
�dB
��B
}�B
jB
YB
>wB
�B	�ZB	��B	�qB	�RB	�9B	��B	�{B	�JB	�%B	|�B	hsB	@�B	2-B	)�B	 �B	DB�B�;B�B��BŢB�wB�^B�FB�-B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�LB�XB�XB�RB�LB��BŢB�#B�sB�mB�B�B�yB�sB�mB�TB�)B��B��BȴBB�^B�LB�3B�B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�'B�qBƨB�B��B��B�B�5B�#B�B��B��B	B	B	  B��B��B��B��B	JB	\B	�B	uB	\B	VB	%B	bB	�B	�B	�B	�B	 �B	$�B	,B	33B	49B	49B	49B	8RB	8RB	7LB	33B	0!B	(�B	"�B	�B	�B	bB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	bB		7B	
=B	JB	�B	�B	�B	�B	!�B	0!B	8RB	6FB	:^B	H�B	W
B	ZB	YB	K�B	F�B	T�B	_;B	_;B	^5B	^5B	[#B	XB	VB	VB	VB	VB	T�B	T�B	T�B	T�B	XB	YB	\)B	^5B	_;B	cTB	e`B	hsB	k�B	k�B	o�B	y�B	~�B	}�B	}�B	|�B	|�B	~�B	�B	�1B	�=B	�=B	�7B	�=B	�=B	�JB	�JB	�JB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	�3B	�3B	�3B	ÖB	B	�qB	�-B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�RB	�RB	�XB	�^B	�wB	��B	��B	ĜB	ƨB	ǮB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ƨB	B	B	��B	��B	��B	ĜB	ĜB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	�B	�#B	�#B	�#B	�)B	�5B	�5B	�;B	�;B	�NB	�ZB	�fB	�fB	�fB	�mB	�mB	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
�B
	7B

=B
{B
�B
(�B
.B
33B
>wB
C�B
I�B
M�B
S�B
W
B
]/B
bNB
gmB
m�B
r�B
u�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�{B
�B
�~B
�~B
�B
�yB
�yB
�zB
�zB
�zB
�yB
�zB
�zB
�zB
�zB
�yB
�{B
�zB
�{B
�yB
�{B
�yB
�BAB�B�B*�BE�Ba9B}�B��B�8B��B�BB�B4+BV�Bo�Bn�Bx�Bw�B�5B��B�'B�eBȧB��B�4B�IB�bB�`B�^B�B��B �B��B��B��B��B��B��B��B �B
B��B�B�hB�fB�[B�B�BŏB��B�_B��B�RB�Bv�Bb?B]BN�BF�B@rB/B�BpB9B��B�)BÅB�[B��Bw�B`2BW�BB�B:NB?nB:QB�B
��B
��B
�WB
��B
}�B
jqB
Y
B
>kB
wB	�NB	ʸB	�hB	�JB	�0B	��B	�tB	�DB	�B	|�B	hpB	@~B	2*B	)�B	 �B	@B�B�<B�B��BšB�wB�^B�GB�.B�#B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�JB�TB�TB�NB�IB��BŠB�B�nB�gB�B�B�vB�nB�hB�PB�&B��B��BȰBB�[B�JB�0B�B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B�$B�kBƤB�B��B��B�B�1B�B�B��B��B	 B	B��B��B��B��B��B	BB	UB	�B	nB	UB	OB	B	\B	�B	}B	�B	�B	 �B	$�B	+�B	3*B	4/B	4/B	4.B	8IB	8JB	7AB	3*B	0B	(�B	"�B	�B	�B	ZB	�B	�B	�B	�B	B	wB	~B	�B	�B	�B	�B	}B	YB		0B	
2B	@B	�B	�B	B	�B	!�B	0B	8HB	6<B	:UB	H�B	V�B	ZB	YB	K�B	F�B	T�B	_/B	_1B	^)B	^*B	[B	XB	U�B	U�B	U�B	U�B	T�B	T�B	T�B	T�B	XB	YB	\B	^(B	_.B	cIB	eVB	hjB	kzB	kzB	o�B	y�B	~�B	}�B	}�B	|�B	|�B	~�B	��B	�#B	�2B	�2B	�)B	�2B	�1B	�;B	�:B	�=B	�VB	�B	�zB	�xB	�sB	��B	��B	��B	��B	��B	��B	�B	�%B	�%B	�#B	ÈB	B	�bB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�BB	�DB	�FB	�RB	�hB	�sB	�|B	čB	ƙB	ǠB	ƘB	˷B	��B	��B	��B	��B	��B	̽B	��B	˷B	˷B	ƖB	�B	B	�{B	�{B	�{B	čB	ċB	ƛB	ǠB	ȤB	ʲB	ʱB	˸B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�+B	�+B	�>B	�IB	�VB	�UB	�UB	�\B	�]B	�]B	�[B	�]B	�eB	�cB	�cB	�mB	�qB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
G�O�B
	&B

+B
jB
�B
(�B
.B
3 B
>bB
C�B
I�B
M�B
S�B
V�B
]B
b=B
gZB
mB
r�B
u�B
z�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.18 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436402016080714364020160807143640  AO  ARCAADJP                                                                    20151023191735    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151023191735  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151023191735  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143640  IP                  G�O�G�O�G�O�                