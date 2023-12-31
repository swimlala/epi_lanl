CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-16T09:17:25Z AOML 3.0 creation; 2016-08-07T21:36:33Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150316091725  20160807143633  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               %A   AO  5286_8897_037                   2C  D   APEX                            6531                            072314                          846 @�A�	J�1   @�Aʕί�@2[��S���c�$�/1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    %A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dyy�D��D�L�D��fD���D� D�6fD���D���D�fD�P D��fD��3D�3D�9�Dڃ3Dਗ਼D�	�D�9�D�3D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�A�HA"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B��\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)BЏ\B�\)B�\)B�\)B�\)B�(�B�\)B�\)B�\)B�\)B��\B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*G�C,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�DtxRDy�D�"�D�R�D��)D�ҐD��D�<)D���D��D�)D�U�D��)D���D��D�?]Dڈ�D�]D�]D�?]D��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�l�A�jA�VA�bA���AȮAȲ-Aȴ9AȶFAȶFAȲ-AȮAȩ�Aȥ�Aȝ�Aș�AȑhAȉ7Aȇ+Aȇ+A�~�A�|�A�x�A�r�A�\)A�S�A�`BA�r�AȋDAȮAȰ!AȶFAɏ\A��A�S�AʅA�ZA�;dA��A�A��`A�ƨAɣ�A�|�A�XA�{A�ȴAȡ�A�r�A���A�oA��;A���A�ȴAÑhA�ȴA�%A��A�~�A�E�A��A���A���A�+A�dZA��^A�-A�1A���A�A�z�A�|�A�E�A��A��mA���A�O�A�v�A�r�A�\)A�^5A��A�$�A��A�K�A�
=A��yA��RA�+A��A��A��A��A�K�A��A���A��A;dA}��A}l�A}p�A}O�A|�A{G�Ax�Au�Aot�Ak|�AhQ�Ac�;Ab�RA_C�A]G�A\�A[S�AVbNAT9XARbNAQƨAO�#AIƨAG�#AE�AC|�AA�A>jA:�jA7/A5�
A3��A2��A1K�A/�FA.�!A-hsA,bNA+XA*bA(ĜA(�A'�A&z�A%O�A$�9A$A#7LA"{A!�A�mAS�A�A�yA�wA`BA�AffA�hA+A�AAhsAS�AS�At�A��Ax�AC�A�RAbA�AĜA��A�\AE�A�Ap�A�wA�FA�A(�A�7AA
=A�AQ�A�A9XA\)A
�yA
ĜA
�\A	A	;dA��A��A�
A�A$�A%@�@�j@��w@��#@��;@���@���@���@�
=@�G�@��@��@��H@�M�@�X@��@�ƨ@���@�\@�^@��@�I�@��y@�Ĝ@���@���@��@�V@ܴ9@�S�@�^5@���@׾w@�v�@���@Ցh@�7L@Լj@�9X@ӍP@�+@���@�@��`@�ƨ@�K�@��@�V@�X@��@̬@�A�@�1@��@ˍP@��@ʏ\@��@�O�@���@��;@���@ƸR@Ƨ�@�v�@�$�@őh@�G�@�/@�`B@�?}@Ĭ@�I�@°!@���@���@�I�@�1'@��;@��P@��P@�dZ@���@�^5@�=q@�$�@�{@�J@���@���@��@�?}@�7L@�7L@��@��@��`@���@��9@��j@�1'@�+@��\@�v�@�V@�E�@���@��!@�5?@��T@��@�1@��`@�@�^5@�-@�O�@��@��D@�ƨ@���@�5?@��^@�hs@�G�@�?}@��@�V@���@��j@�bN@��j@��`@�r�@�(�@���@�@�$�@��T@��^@�p�@�Ĝ@� �@�t�@�+@��R@�-@��@�@���@���@��7@��@��@���@��u@���@�1@�\)@�;d@��@�~�@�=q@��@��#@���@�7L@��/@��@�9X@��@��w@���@�"�@��!@�-@���@�p�@�7L@���@��@�j@�b@���@���@�l�@��!@�$�@���@��@��9@�1@���@��@���@���@���@�n�@�-@�5?@�5?@�5?@��@��^@�X@�V@���@�z�@�Q�@�I�@� �@��
@���@�;d@�ȴ@�v�@�M�@�E�@��@�@���@���@�`B@�G�@�?}@�&�@��`@��j@��@��@��u@�r�@� �@��
@���@�S�@�"�@��@���@�^5@��T@�`B@�?}@�7L@�7L@��@���@���@�z�@�bN@���@�\)@��R@�^5@���@�V@���@��@��u@��u@���@��u@��u@�z�@� �@�ƨ@���@��@�\)@�"�@��y@�ff@��@��@��@���@��h@�p�@���@��j@���@��@�9X@��m@��F@���@�C�@�o@��@�33@|j@q7L@g�@a��@[��@Q��@IX@AG�@:�@5/@0A�@*��@$j@
=@M�@�@Ĝ@�@	&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�n�A�l�A�jA�VA�bA���AȮAȲ-Aȴ9AȶFAȶFAȲ-AȮAȩ�Aȥ�Aȝ�Aș�AȑhAȉ7Aȇ+Aȇ+A�~�A�|�A�x�A�r�A�\)A�S�A�`BA�r�AȋDAȮAȰ!AȶFAɏ\A��A�S�AʅA�ZA�;dA��A�A��`A�ƨAɣ�A�|�A�XA�{A�ȴAȡ�A�r�A���A�oA��;A���A�ȴAÑhA�ȴA�%A��A�~�A�E�A��A���A���A�+A�dZA��^A�-A�1A���A�A�z�A�|�A�E�A��A��mA���A�O�A�v�A�r�A�\)A�^5A��A�$�A��A�K�A�
=A��yA��RA�+A��A��A��A��A�K�A��A���A��A;dA}��A}l�A}p�A}O�A|�A{G�Ax�Au�Aot�Ak|�AhQ�Ac�;Ab�RA_C�A]G�A\�A[S�AVbNAT9XARbNAQƨAO�#AIƨAG�#AE�AC|�AA�A>jA:�jA7/A5�
A3��A2��A1K�A/�FA.�!A-hsA,bNA+XA*bA(ĜA(�A'�A&z�A%O�A$�9A$A#7LA"{A!�A�mAS�A�A�yA�wA`BA�AffA�hA+A�AAhsAS�AS�At�A��Ax�AC�A�RAbA�AĜA��A�\AE�A�Ap�A�wA�FA�A(�A�7AA
=A�AQ�A�A9XA\)A
�yA
ĜA
�\A	A	;dA��A��A�
A�A$�A%@�@�j@��w@��#@��;@���@���@���@�
=@�G�@��@��@��H@�M�@�X@��@�ƨ@���@�\@�^@��@�I�@��y@�Ĝ@���@���@��@�V@ܴ9@�S�@�^5@���@׾w@�v�@���@Ցh@�7L@Լj@�9X@ӍP@�+@���@�@��`@�ƨ@�K�@��@�V@�X@��@̬@�A�@�1@��@ˍP@��@ʏ\@��@�O�@���@��;@���@ƸR@Ƨ�@�v�@�$�@őh@�G�@�/@�`B@�?}@Ĭ@�I�@°!@���@���@�I�@�1'@��;@��P@��P@�dZ@���@�^5@�=q@�$�@�{@�J@���@���@��@�?}@�7L@�7L@��@��@��`@���@��9@��j@�1'@�+@��\@�v�@�V@�E�@���@��!@�5?@��T@��@�1@��`@�@�^5@�-@�O�@��@��D@�ƨ@���@�5?@��^@�hs@�G�@�?}@��@�V@���@��j@�bN@��j@��`@�r�@�(�@���@�@�$�@��T@��^@�p�@�Ĝ@� �@�t�@�+@��R@�-@��@�@���@���@��7@��@��@���@��u@���@�1@�\)@�;d@��@�~�@�=q@��@��#@���@�7L@��/@��@�9X@��@��w@���@�"�@��!@�-@���@�p�@�7L@���@��@�j@�b@���@���@�l�@��!@�$�@���@��@��9@�1@���@��@���@���@���@�n�@�-@�5?@�5?@�5?@��@��^@�X@�V@���@�z�@�Q�@�I�@� �@��
@���@�;d@�ȴ@�v�@�M�@�E�@��@�@���@���@�`B@�G�@�?}@�&�@��`@��j@��@��@��u@�r�@� �@��
@���@�S�@�"�@��@���@�^5@��T@�`B@�?}@�7L@�7L@��@���@���@�z�@�bN@���@�\)@��R@�^5@���@�V@���@��@��u@��u@���@��u@��u@�z�@� �@�ƨ@���@��@�\)@�"�@��y@�ff@��@��@��@���@��h@�p�@���@��j@���@��@�9X@��m@��F@���@�C�G�O�@��@�33@|j@q7L@g�@a��@[��@Q��@IX@AG�@:�@5/@0A�@*��@$j@
=@M�@�@Ĝ@�@	&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	6FB	6FB	6FB	7LB	9XB	D�B	N�B	VB	ZB	\)B	_;B	e`B	hsB	jB	k�B	l�B	m�B	n�B	o�B	p�B	p�B	q�B	s�B	u�B	w�B	}�B	�B	�+B	�VB	��B	�B	�!B	�FB
E�B
��B
�?B
�yBhB+B>wBO�BgmB{�B�PB��B�BB�5B�B�B��BPB\B
=BB��B��B��B��Bo�Bn�Bv�Bt�Br�Bp�Bl�Bm�Bl�Bk�BgmB_;BM�BE�B:^B-B�BPB�B33BC�BQ�B��B��B�\B�Bp�Bk�BiyBe`B\)BM�BG�B/B
=B
��B
G�B
+B	ȴB	�B	��B	��B	��B	��B	��B	��B	�=B	{�B	hsB	S�B	G�B	33B	+B	�B	{B	bB	B�B�B�ZB�;B��BĜB�wBBÖB�wB�3B��B��B��B��B��B��B��B��B��B�B�'B�9B�RB�XB�^B�wBĜBƨBȴB��B��B��B��B�
B�
B�B�5B�TB�ZB�`B�yB�B�B��B��B��B��B	DB	�B	(�B	+B	(�B	)�B	-B	/B	/B	/B	33B	?}B	G�B	P�B	\)B	YB	VB	VB	L�B	G�B	E�B	D�B	A�B	:^B	6FB	2-B	1'B	1'B	33B	-B	!�B	�B	bB	1B	B�B�TB�;B�;B�)B�#B�B�B�fB�ZB�`B�yB�B�B�B�B�B�B�B�B�B�yB�ZB�ZB�HB�NB�mB�mB�yB�B�B�B�B�B��B��B��B��B��B	  B	B	B	
=B	JB	JB	PB	\B	VB	\B	uB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	'�B	+B	+B	+B	+B	,B	.B	8RB	;dB	=qB	A�B	C�B	B�B	A�B	>wB	?}B	D�B	H�B	H�B	K�B	M�B	N�B	O�B	S�B	W
B	XB	XB	ZB	\)B	_;B	aHB	cTB	dZB	e`B	iyB	jB	p�B	t�B	s�B	t�B	v�B	x�B	x�B	x�B	{�B	�B	�B	�JB	�hB	�bB	�oB	�hB	�oB	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�3B	�3B	�9B	�LB	�dB	�dB	�jB	�jB	�qB	B	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�HB	�HB	�HB	�TB	�ZB	�ZB	�mB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
+B
1B
1B
1B
1B
1B
DB
PB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
hB
oB
uB
{B
�B
$�B
(�B
.B
5?B
:^B
B�B
H�B
M�B
R�B
W
B
\)B
`BB
dZB
iyB
m�B
r�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	6KB	6IB	6GB	7LB	9YB	D�B	N�B	VB	Z B	\+B	_;B	e`B	htB	j~B	k�B	l�B	m�B	n�B	o�B	p�B	p�B	q�B	s�B	u�B	w�B	}�B	�B	�)B	�RB	��B	�B	� B	�EB
E�B
�{B
�3B
�kBZB*�B>kBO�Bg`B{�B�AB��B� BB�&B�mB�B��BBBNB
.BB��B��B�uB��Bo�Bn�Bv�Bt�Br�Bp�Bl}BmBl|BktBg\B_,BM�BE�B:RB,�B�BAB�B3#BC�BQ�B��B��B�JB��Bp�BksBifBeNB\BM�BG�B/
B
+B
��B
G�B
 B	ȪB	�B	��B	��B	��B	��B	��B	��B	�4B	{�B	hnB	S�B	G�B	3/B	*�B	�B	xB	_B	B�B�|B�XB�<B��BĝB�vBB×B�wB�2B��B��B��B��B��B��B��B��B��B�B�&B�8B�QB�WB�^B�vBĝBƥBȳB��B��B��B��B�B�B�B�4B�RB�WB�[B�uB�B�B��B��B��B��B	@B	�B	(�B	*�B	(�B	)�B	-B	/B	/B	/B	3+B	?uB	G�B	P�B	\B	YB	U�B	U�B	L�B	G�B	E�B	D�B	A�B	:UB	6<B	2$B	1B	1B	3*B	-B	!�B	�B	\B	)B	B�B�PB�6B�5B�#B�B� B�
B�bB�TB�[B�sB�B�B�B�B�B�B�B�B�B�sB�TB�VB�DB�JB�eB�gB�rB�}B�B�B�B�B��B��B��B��B��B��B	 B	B	
3B	AB	CB	GB	VB	MB	SB	jB	yB	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	'�B	*�B	*�B	*�B	*�B	+�B	.B	8FB	;[B	=hB	AB	C�B	B�B	A�B	>jB	?tB	D�B	H�B	H�B	K�B	M�B	N�B	O�B	S�B	V�B	XB	XB	ZB	\ B	_0B	a<B	cIB	dMB	eRB	imB	jsB	p�B	t�B	s�B	t�B	v�B	x�B	x�B	x�B	{�B	�B	�B	�>B	�]B	�VB	�bB	�XB	�bB	��B	��B	�B	�B	�B	�B	�B	�B	� B	�$B	�%B	�&B	�)B	�>B	�YB	�WB	�[B	�\B	�bB	B	œB	ǟB	ʲB	̽B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�!B	�$B	�*B	�,B	�4B	�1B	�7B	�9B	�9B	�DB	�JB	�HB	�^B	�[B	�\B	�cB	�oB	�uB	�vB	�|B	�|B	�B	�B	�{B	�vB	�vB	�uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
 �B
 �B
 �B
�B
	B
B
B
"B
!B
B
!B
!B
1B
=B
EB
CB
KB
KB
KB
LB
PB
PB
PB
QB
UG�O�B
cB
gB
�B
$�B
(�B
.B
5,B
:KB
B|B
H�B
M�B
R�B
V�B
\B
`0B
dEB
igB
m}B
r�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.18 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436332016080714363320160807143633  AO  ARCAADJP                                                                    20150316091725    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150316091725  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150316091725  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143633  IP                  G�O�G�O�G�O�                