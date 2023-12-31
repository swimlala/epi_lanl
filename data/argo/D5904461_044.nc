CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-22T09:16:18Z AOML 3.0 creation; 2016-08-07T21:36:34Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150422091618  20160807143634  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               ,A   AO  5286_8897_044                   2C  D   APEX                            6531                            072314                          846 @�K��@
1   @�Kl�@2�ě��T�c����l�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    ,A   B   B   @&ff@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyL�D���D�L�D��fD�ٚD�	�D�0 D��3D�� D�fD�6fD�ffD�� D�fD�P Dډ�D���D�3D�0 D�i�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @1G�@�p�@�p�A�A"�RAB�RAb�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�BizBp�Bx�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(EC*EC,+�C.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
�D ��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D	
�D	��D

�D
��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D
�D��D 
�D ��D!
�D!��D"
�D"��D#
�D#��D$
�D$��D%
�D%��D&
�D&��D'
�D'��D(
�D(��D)
�D)��D*
�D*��D+
�D+��D,
�D,��D-
�D-��D.
�D.��D/
�D/��D0
�D0��D1
�D1��D2
�D2��D3
�D3��D4
�D4��D5
�D5��D6
�D6��D7
�D7��D8
�D8��D9
�D9��D:
�D:��D;
�D;��D<
�D<��D=
�D=��D>
�D>�{D?
�D?��D@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG
�DG��DH
�DH��DI
�DI��DJ
�DJ��DK
�DK��DL
�DL��DM
�DM��DN
�DN��DO
�DO��DP
�DP��DQ
�DQ��DR
�DR��DS
�DS��DT
�DT��DU
�DU��DV
�DV��DW
�DW��DX
�DX��DY
�DY��DZ
�DZ��D[
�D[��D\
�D\��D]
�D]��D^
�D^��D_
�D_��D`
�D`��Da
�Da��Db
�Db��Dc
�Dc��Dd
�Dd��De
�De��Df
�Df��Dg
�Dg��Dh
�Dh��Di
�Di��Dj
�Dj��Dk
�Dk��Dl
�Dl��Dm
�Dm��Dn
�Dn��Do
�Do��Dp
�Dp��Dq
�Dq��Dr
�Dr��Ds
�Ds��Dt
�DtqGDyW�D��>D�R>D���D��D�D�5qD���D��qD��D�;�D�k�D��qD��D�UqDڏD��>D��D�5qD�oD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��;A��TA��HA��#A��`A��TA��
AʑhAʅA�jA�`BA�^5A�^5A�bNA�ffA�ffA�jA�p�A�x�A�z�AʃAʍPAʝ�Aʣ�AʮAʸRAʶFAʮAʋDA�jA�t�Aʥ�AʼjAʺ^A�~�A�^5A�A�A�A���A���A�\)A��A�v�A���AƁA�O�A�5?A�A��A���AŶFAŧ�Aś�Aŉ7AŅA�n�A�AľwAô9A�A�v�A���A�+A�7LA��/A�{A�v�A���A�+A��FA���A�ffA�S�A���A���A�+A�A�jA�C�A�-A��A��jA�A�A��mA��A�;dA�jA���A��A��A�^5A���A�$�A���A���A�v�A�{A���A��#A���A�M�A�|�A�t�A��;A��mA��A���A�-A�;dA��A�ĜA��;A��uA�~�A��TA�~�A�hsA�33A��A��A�1A�O�AwO�Au�AsdZAs7LAr��Ao?}Aj1'Ag"�Ad~�AbVAa�^A`Q�A^�uA]��A\ĜA[�FA[oAZ�uAZ{AY��AX�RAU�AQ��AOoAM�FAK�
AJ��AJVAI�AH�\AF�jAE+ABĜAAƨA@�A?ƨA;�TA9�A9oA7��A6z�A5x�A4�9A2M�A0(�A-�A,��A*�yA*jA*I�A)�^A({A%�mA$�jA$$�A#A#�A#"�A"�`A"�A!��A  �A�7A��A��AĜAM�A �A��A��A�uAƨA1'A�#A"�AAK�A��A\)A��A�9A�PA��Ap�A
�A	�TA	S�An�A33A�!A1'AZA+A�uAZA$�A �A �@���@�~�@�v�@�n�@���@�j@��\@�&�@�Q�@���@�o@��@�r�@��@�ȴ@�%@@�@�j@畁@�@�|�@�S�@柾@�u@�$�@�t�@�~�@�?}@܃@�ƨ@ڗ�@�x�@���@��
@�;d@�^5@���@ԓu@���@���@�Z@�1@ύP@��H@·+@�-@�J@�`B@�9X@�v�@�X@�`B@�X@ȼj@�9X@� �@��@ǶF@�t�@�33@��@�$�@ŉ7@�V@ēu@�(�@öF@��@\@��@�O�@�hs@��7@�J@�;d@�"�@�|�@��@�t�@�K�@���@�{@���@�1@öF@�33@���@�dZ@Å@���@�5?@�{@���@���@��@�1'@�\)@�`B@���@�I�@���@�\)@��y@��@���@���@��^@�@�@���@��7@�?}@���@�9X@�1@��@���@�K�@�-@��#@��-@�X@�%@�1'@�M�@�%@���@���@�x�@�;d@�^5@��@��-@�hs@�G�@�G�@�%@��@�j@�Q�@�9X@���@�C�@�ȴ@�M�@��@��-@��7@�hs@���@���@���@�r�@�Q�@�1@��@��P@��@�t�@�dZ@�K�@�33@�"�@��y@��@���@���@���@���@�ȴ@��@�\)@��@��@�t�@�l�@�K�@�C�@�33@��H@�v�@�$�@��#@��-@��7@��@�x�@�`B@�G�@�/@�%@���@���@�%@���@���@��@�9X@� �@�  @��;@��@��P@�\)@�;d@���@��@�J@��@��T@���@��h@��7@���@��-@��^@��-@���@��@�G�@�%@��D@�1@�K�@��@��+@�^5@�5?@��@�X@��@���@��m@���@�t�@�K�@��y@���@�n�@�5?@��T@��h@�G�@�/@��@��@��@��@��@��u@�Q�@���@�ƨ@��@��P@�|�@�\)@�K�@��@���@�v�@�-@�{@�@��@�@���@�C�@���@��9@u�@mp�@d�/@W�;@Q7L@J=q@@bN@;�
@6@0b@+@&�y@#�
@�@;d@��@��@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��TA��;A��TA��HA��#A��`A��TA��
AʑhAʅA�jA�`BA�^5A�^5A�bNA�ffA�ffA�jA�p�A�x�A�z�AʃAʍPAʝ�Aʣ�AʮAʸRAʶFAʮAʋDA�jA�t�Aʥ�AʼjAʺ^A�~�A�^5A�A�A�A���A���A�\)A��A�v�A���AƁA�O�A�5?A�A��A���AŶFAŧ�Aś�Aŉ7AŅA�n�A�AľwAô9A�A�v�A���A�+A�7LA��/A�{A�v�A���A�+A��FA���A�ffA�S�A���A���A�+A�A�jA�C�A�-A��A��jA�A�A��mA��A�;dA�jA���A��A��A�^5A���A�$�A���A���A�v�A�{A���A��#A���A�M�A�|�A�t�A��;A��mA��A���A�-A�;dA��A�ĜA��;A��uA�~�A��TA�~�A�hsA�33A��A��A�1A�O�AwO�Au�AsdZAs7LAr��Ao?}Aj1'Ag"�Ad~�AbVAa�^A`Q�A^�uA]��A\ĜA[�FA[oAZ�uAZ{AY��AX�RAU�AQ��AOoAM�FAK�
AJ��AJVAI�AH�\AF�jAE+ABĜAAƨA@�A?ƨA;�TA9�A9oA7��A6z�A5x�A4�9A2M�A0(�A-�A,��A*�yA*jA*I�A)�^A({A%�mA$�jA$$�A#A#�A#"�A"�`A"�A!��A  �A�7A��A��AĜAM�A �A��A��A�uAƨA1'A�#A"�AAK�A��A\)A��A�9A�PA��Ap�A
�A	�TA	S�An�A33A�!A1'AZA+A�uAZA$�A �A �@���@�~�@�v�@�n�@���@�j@��\@�&�@�Q�@���@�o@��@�r�@��@�ȴ@�%@@�@�j@畁@�@�|�@�S�@柾@�u@�$�@�t�@�~�@�?}@܃@�ƨ@ڗ�@�x�@���@��
@�;d@�^5@���@ԓu@���@���@�Z@�1@ύP@��H@·+@�-@�J@�`B@�9X@�v�@�X@�`B@�X@ȼj@�9X@� �@��@ǶF@�t�@�33@��@�$�@ŉ7@�V@ēu@�(�@öF@��@\@��@�O�@�hs@��7@�J@�;d@�"�@�|�@��@�t�@�K�@���@�{@���@�1@öF@�33@���@�dZ@Å@���@�5?@�{@���@���@��@�1'@�\)@�`B@���@�I�@���@�\)@��y@��@���@���@��^@�@�@���@��7@�?}@���@�9X@�1@��@���@�K�@�-@��#@��-@�X@�%@�1'@�M�@�%@���@���@�x�@�;d@�^5@��@��-@�hs@�G�@�G�@�%@��@�j@�Q�@�9X@���@�C�@�ȴ@�M�@��@��-@��7@�hs@���@���@���@�r�@�Q�@�1@��@��P@��@�t�@�dZ@�K�@�33@�"�@��y@��@���@���@���@���@�ȴ@��@�\)@��@��@�t�@�l�@�K�@�C�@�33@��H@�v�@�$�@��#@��-@��7@��@�x�@�`B@�G�@�/@�%@���@���@�%@���@���@��@�9X@� �@�  @��;@��@��P@�\)@�;d@���@��@�J@��@��T@���@��h@��7@���@��-@��^@��-@���@��@�G�@�%@��D@�1@�K�@��@��+@�^5@�5?@��@�X@��@���@��m@���@�t�@�K�@��y@���@�n�@�5?@��T@��h@�G�@�/@��@��@��@��@��@��u@�Q�@���@�ƨ@��@��P@�|�@�\)@�K�@��@���@�v�@�-@�{@�@��@�G�O�@�C�@���@��9@u�@mp�@d�/@W�;@Q7L@J=q@@bN@;�
@6@0b@+@&�y@#�
@�@;d@��@��@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�dB	�dB	�dB	�jB	�dB	�jB	�jB	�dB	�XB	�XB	�dB	�}B	��B	ÖB	ƨB	��B	��B	��B	�#B	�mB	�sB	�B	�B	��B	��B	��B	��B
PB
hB
�B
/B
E�B
�B
�dB
�NB6FBF�BM�BiyB�uB��B�BǮB�LB�B��B�9B�jB��B��B�B�B��B  B
=B{B1'BM�BT�Be`BgmBn�B{�B�B~�Be`BN�B0!B!�B�BoB6FBffBq�B� B�B�B|�Bt�Br�Bq�Bm�Bu�BgmBF�B �B%BB�BK�B/B�)B�!B��Bq�Bs�B��B��B�oB��B��B�DBw�BD�B{B
��B
��B
�\B
p�B
jB
]/B
iyB
v�B
s�B
~�B
~�B
�B
�JB
�PB
�bB
� B
gmB
,B	�#B	ƨB	�?B	ƨB	ÖB	��B	aHB	G�B	8RB	7LB	=qB	6FB	&�B	�B	�B	bB	
=B	+B	B	  B��B�B�B��B��B��BƨBĜBÖBĜB��B�jB�jB�dB�^B�LB�XB�dB�dB�dB�jB�wB�}BƨB��B��B�B�
B�B��B��B��B��B��BɺBɺBɺB��B��B��BȴBŢBÖBBÖBÖB��BÖBĜBŢBĜB��BĜBǮB��B��B��B��B��B��B��B��B��B��B��B��B��BȴBƨBŢBÖBB��B��B��B��BŢB��B��B��B��B��B�
B�B�B��B��B�B�
B�B�/B�;B�BB�HB�BB�B�B�B�B�B�B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�)B�/B�5B�5B�;B�;B�5B�/B�5B�NB�TB�`B�yB�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	+B	JB	{B	�B	�B	�B	(�B	A�B	J�B	J�B	N�B	Q�B	VB	VB	XB	XB	ZB	ZB	[#B	^5B	`BB	`BB	`BB	_;B	_;B	^5B	]/B	[#B	YB	\)B	dZB	q�B	u�B	w�B	x�B	~�B	�B	�B	�%B	�%B	�+B	�7B	�DB	�PB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�3B	�!B	�-B	�'B	�B	��B	�B	�B	�-B	�3B	�9B	�?B	�?B	�?B	�FB	�FB	�LB	�RB	�RB	�XB	�XB	�^B	�dB	�wB	�wB	�qB	�qB	�qB	�qB	�qB	�wB	��B	B	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�)B	�/B	�;B	�BB	�BB	�BB	�HB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
+B
%B
bB
�B
�B
$�B
)�B
49B
9XB
=qB
E�B
I�B
N�B
S�B
XB
]/B
_;B
e`B
k�B
o�B
r�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�aB	�dB	�dB	�gB	�eB	�gB	�iB	�bB	�SB	�TB	�aB	�zB	��B	ÓB	ƤB	��B	��B	��B	�B	�kB	�pB	�B	�B	��B	��B	��B	��B
KB
`B
�B
/B
E�B
�	B
�[B
�DB67BF�BM�BijB�gB��B�
BǠB�>B��B��B�.B�ZB̾B��B�	B�B��B��B
0BmB1BM�BT�BeNBgaBn�B{�B�B~�BePBN�B0B!�B�B_B69BfXBq�B�B�B�B|�Bt�Br�Bq�Bm�Bu�Bg]BF�B �BB �BrBK�B/B�B�B��Bq�Bs�B��B��B�^B��B��B�3Bw�BD�BmB
��B
��B
�NB
p�B
jsB
]"B
inB
v�B
s�B
~�B
~�B
�B
�>B
�CB
�RB
�B
gaB
+�B	�B	ƢB	�7B	ƠB	ÍB	��B	aBB	G�B	8OB	7KB	=lB	6BB	&�B	�B	�B	_B	
:B	'B	B��B��B�B�B��B��B��BƩBĜB×BğB��B�jB�mB�gB�aB�LB�XB�fB�fB�dB�jB�wB�|BƥB��B��B�B�B�B� B��B��B��B��BɸBɷBɹB��B��B��BȵBŢBÕBBÔBÖB��BÓBěBŠBĜB��BĜBǪB��B��B��B��B��B��B��B��B��B��B��B��B��BȲBƦBŢBÓBB��B��B��B�~BşB��B��B��B��B��B�B�B�B��B��B�B�B�B�+B�7B�>B�CB�>B�B�B�B�B�B�B�B��B��B��B��BʼBʾB��B��B��B��B��B��B��B��B��B��B�B�B�%B�+B�/B�/B�6B�4B�/B�*B�.B�KB�OB�[B�sB�B�B�~B�B�B�B�B�B��B��B��B��B��B	B	%B	BB	tB	�B	�B	�B	(�B	AB	J�B	J�B	N�B	Q�B	U�B	U�B	XB	XB	ZB	ZB	[B	^+B	`7B	`:B	`7B	_1B	_1B	^+B	]$B	[B	YB	\B	dQB	q�B	u�B	w�B	x�B	~�B	�B	�B	�B	�B	�!B	�-B	�:B	�CB	�gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�+B	�(B	�B	� B	�B	��B	��B	��B	�B	�!B	�$B	�,B	�3B	�2B	�1B	�8B	�6B	�@B	�DB	�DB	�JB	�IB	�RB	�WB	�jB	�jB	�bB	�cB	�dB	�bB	�dB	�kB	�yB	B	ǠB	ʵB	ʳB	ʴB	̽B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�6B	�4B	�8B	�:B	�?B	�=B	�EB	�FB	�DB	�EB	�DB	�GB	�DB	�IB	�LB	�LB	�LB	�LB	�LB	�LB	�RB	�QB	�QB	�SB	�RB	�VB	�YB	�WB	�_B	�_B	�eB	�eB	�eB	�cB	�kB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
B
B
B
G�O�B
B
RB
�B
�B
$�B
)�B
4(B
9HB
=aB
E�B
I�B
N�B
S�B
W�B
]B
_(B
eOB
ksB
o�B
r�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436342016080714363420160807143634  AO  ARCAADJP                                                                    20150422091618    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150422091618  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150422091618  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143634  IP                  G�O�G�O�G�O�                