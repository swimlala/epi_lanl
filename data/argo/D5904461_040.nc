CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-01T02:15:57Z AOML 3.0 creation; 2016-08-07T21:36:34Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150401021557  20160807143634  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               (A   AO  5286_8897_040                   2C  D   APEX                            6531                            072314                          846 @�E�[���1   @�E��>�	@2���-V�c�p��
=1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    (A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D��D�FfD�|�D�ٚD�  D�0 D��fD��fD�	�D�9�D�l�D��fD�3D�FfD�l�D�ٚD�fD�FfD�|�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@�p�A�RA"�RAB�RAb�RA�\)A�\)A�\)A�\)A��\A�\)A�\)A�\)B �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�W
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
B�=B�W
B�W
C +�C+�C+�C+�C+�C
+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C+�C +�C"+�C$+�C&+�C(+�C*+�C,EC.+�C0+�C2+�C4+�C6+�C8+�C:+�C<+�C>+�C@+�CB+�CD+�CF+�CH+�CJ+�CL+�CN+�CP+�CR+�CT+�CV+�CX+�CZ+�C\+�C^+�C`+�Cb+�Cd+�Cf+�Ch+�Cj+�Cl+�Cn+�Cp+�Cr+�Ct+�Cv+�Cx+�Cz+�C|+�C~+�C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�"�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D 
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
�D ��D!GD!��D"
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
�D>��D?
�D?��D@
�D@��DA
�DA��DB
�DB��DC
�DC��DD
�DD��DE
�DE��DF
�DF��DG{DG��DH
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
�Dt��Dt�GDy��D�D�K�D��>D��D�qD�5qD���D���D�D�?D�r>D���D��D�K�D�r>D��D��D�K�D�>D��>111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AΥ�A�G�A��A���A�ĜAͺ^AͲ-Aͧ�Aͥ�A͟�A�l�A�ĜA��Aʡ�A�r�A�;dAɬA�n�A�r�A�|�A�v�A�r�A�\)A�1'A�1'A�9XA�Q�A�XAɣ�A�jA�S�Aǟ�A�+A���A��Aơ�A�|�A�ffAŮA�Aģ�A�bNA�K�A�oA�A���A×�A�-A��
A�`BA�C�A��A��A���A��yA��A�x�A�C�A���A�z�A�`BA��-A�n�A��
A��A�9XA��HA�K�A�JA�ƨA�/A��\A���A���A��A��jA�hsA��wA�dZA���A��A�JA�%A���A�n�A���A��A�=qA�1A��RA���A�^5A��#A�A�1'A��A��A��HA��/A�;dA�`BA��!A�9XA��A�v�A�PAx�RAu�At�`As?}AjȴAd��Ab5?A`=qA_VA\M�AW��AU�AS��AS\)AR �AQoAO�AM`BAK+AIp�AH�!AH �AGO�AE�AD�`AC&�AA33A@A?p�A>��A=�-A8VA4v�A3t�A1�A/��A/C�A-dZA+��A)p�A'x�A&z�A%�mA#�wA#�A"~�A!
=A bNA��AVA�hAoA^5A�
AS�A~�A\)AVA�A��At�A;dA��At�A��A�\A1'A�TA��A�AĜA�9A��AI�A�-A�AbAp�A33A�!AbA��A�A/A�uAQ�A��A�PA
=A
��A
n�A
Q�A
A	��A	|�A	C�A	oA�A��AVA?}A9XA?}A�AbA�7A&�A�HAn�A��A v�A {@���@��y@�5?@��@�/@��@���@�r�@��@��-@��@�\@�J@�5?@��T@�@�/@���@�z�@���@�C�@�V@�Z@�@�C�@�;d@��@��@�n�@�=q@�D@�C�@�
=@�-@�&�@���@�Q�@�l�@��@�\@�E�@�@�^@�p�@�`B@�(�@�+@�33@�|�@�S�@�E�@���@ݑh@�X@���@܃@ڸR@�1'@�t�@ו�@�
=@�bN@�@���@���@ԋD@ԛ�@�
=@�V@�@�o@�n�@�@��
@��@ǝ�@���@�M�@ŉ7@�X@�7L@�/@��m@�/@�p�@���@þw@���@°!@�E�@�-@�$�@��^@��@�o@��!@�~�@�ff@�^5@�J@�7L@��/@�j@���@���@�
=@�^5@�$�@�p�@���@��9@�j@�  @���@�t�@�33@���@�v�@�J@�@���@�{@�@��^@���@�/@��/@��@�(�@�  @��w@�\)@�K�@�33@�
=@���@���@�@�hs@��@�%@��9@�1@���@�l�@��@��@���@�^5@��#@��7@�7L@�%@�Ĝ@��@�j@�1'@��;@��F@�"�@�@���@���@�5?@���@��@�G�@��`@��j@�9X@���@�ƨ@�dZ@�"�@��@��@���@�V@�@�@��@�X@�G�@�7L@���@�Q�@�b@��;@���@�C�@�@��R@�~�@�n�@�^5@�$�@���@���@�x�@�/@���@�V@��@�bN@��;@�|�@�;d@�o@���@��+@�E�@��@��7@�`B@�/@���@�Z@��@��F@�t�@�dZ@�\)@��@�n�@�J@��#@��^@��-@��h@�hs@�p�@�G�@�/@�7L@��j@� �@���@�dZ@��@�K�@�t�@�;d@�~�@�
=@��@�ȴ@��H@�+@�ȴ@�-@��-@���@�/@�%@���@�Q�@�I�@� �@��
@�\)@��y@�M�@�G�@���@�I�@��@��
@���@���@�C�@�
=@�-@�Ĝ@�ƨ@z^5@o;d@e�@^E�@U�@Nȴ@G��@A�@;�@4Z@/
=@+o@%?}@�P@(�@�;@C�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AΥ�A�G�A��A���A�ĜAͺ^AͲ-Aͧ�Aͥ�A͟�A�l�A�ĜA��Aʡ�A�r�A�;dAɬA�n�A�r�A�|�A�v�A�r�A�\)A�1'A�1'A�9XA�Q�A�XAɣ�A�jA�S�Aǟ�A�+A���A��Aơ�A�|�A�ffAŮA�Aģ�A�bNA�K�A�oA�A���A×�A�-A��
A�`BA�C�A��A��A���A��yA��A�x�A�C�A���A�z�A�`BA��-A�n�A��
A��A�9XA��HA�K�A�JA�ƨA�/A��\A���A���A��A��jA�hsA��wA�dZA���A��A�JA�%A���A�n�A���A��A�=qA�1A��RA���A�^5A��#A�A�1'A��A��A��HA��/A�;dA�`BA��!A�9XA��A�v�A�PAx�RAu�At�`As?}AjȴAd��Ab5?A`=qA_VA\M�AW��AU�AS��AS\)AR �AQoAO�AM`BAK+AIp�AH�!AH �AGO�AE�AD�`AC&�AA33A@A?p�A>��A=�-A8VA4v�A3t�A1�A/��A/C�A-dZA+��A)p�A'x�A&z�A%�mA#�wA#�A"~�A!
=A bNA��AVA�hAoA^5A�
AS�A~�A\)AVA�A��At�A;dA��At�A��A�\A1'A�TA��A�AĜA�9A��AI�A�-A�AbAp�A33A�!AbA��A�A/A�uAQ�A��A�PA
=A
��A
n�A
Q�A
A	��A	|�A	C�A	oA�A��AVA?}A9XA?}A�AbA�7A&�A�HAn�A��A v�A {@���@��y@�5?@��@�/@��@���@�r�@��@��-@��@�\@�J@�5?@��T@�@�/@���@�z�@���@�C�@�V@�Z@�@�C�@�;d@��@��@�n�@�=q@�D@�C�@�
=@�-@�&�@���@�Q�@�l�@��@�\@�E�@�@�^@�p�@�`B@�(�@�+@�33@�|�@�S�@�E�@���@ݑh@�X@���@܃@ڸR@�1'@�t�@ו�@�
=@�bN@�@���@���@ԋD@ԛ�@�
=@�V@�@�o@�n�@�@��
@��@ǝ�@���@�M�@ŉ7@�X@�7L@�/@��m@�/@�p�@���@þw@���@°!@�E�@�-@�$�@��^@��@�o@��!@�~�@�ff@�^5@�J@�7L@��/@�j@���@���@�
=@�^5@�$�@�p�@���@��9@�j@�  @���@�t�@�33@���@�v�@�J@�@���@�{@�@��^@���@�/@��/@��@�(�@�  @��w@�\)@�K�@�33@�
=@���@���@�@�hs@��@�%@��9@�1@���@�l�@��@��@���@�^5@��#@��7@�7L@�%@�Ĝ@��@�j@�1'@��;@��F@�"�@�@���@���@�5?@���@��@�G�@��`@��j@�9X@���@�ƨ@�dZ@�"�@��@��@���@�V@�@�@��@�X@�G�@�7L@���@�Q�@�b@��;@���@�C�@�@��R@�~�@�n�@�^5@�$�@���@���@�x�@�/@���@�V@��@�bN@��;@�|�@�;d@�o@���@��+@�E�@��@��7@�`B@�/@���@�Z@��@��F@�t�@�dZ@�\)@��@�n�@�J@��#@��^@��-@��h@�hs@�p�@�G�@�/@�7L@��j@� �@���@�dZ@��@�K�@�t�@�;d@�~�@�
=@��@�ȴ@��H@�+@�ȴ@�-@��-@���@�/@�%@���@�Q�@�I�@� �@��
@�\)@��y@�M�@�G�@���@�I�@��@��
@���@���@�C�G�O�@�-@�Ĝ@�ƨ@z^5@o;d@e�@^E�@U�@Nȴ@G��@A�@;�@4Z@/
=@+o@%?}@�P@(�@�;@C�@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	ffB	e`B	dZB	dZB	dZB	e`B	e`B	e`B	e`B	e`B	{�B	��B	�}B	ɺB	��B	��B	�#B	�B
%B
'�B
7LB
S�B
\)B
x�B
�bB
�B
�HB�BYB��B�'B�-B��B��B��B�fB�B�B��BJBbBbB.BB�BVBgmBm�BiyBhsBhsBv�B�+B�oB��B�wB��B�B�`B�/B��B�B�B�)B�/B�#B��B�{B�B�B�B��B�uB�wB�/B�BW
BO�B
=B�mB��B�HB��B�9B��B�Bu�BS�B-B{B
��B
�/B
�qB
��B
�B
aHB
T�B
N�B
A�B
%�B
oB
B
B
B	��B	��B	�NB	�B	�uB	�%B	r�B	C�B	$�B	�B	\B		7B��B�B�fB�BB�/B�B��BɺBĜB�}B�wB�wB�qB�dB�dB�XB�XB�RB�FB�-B�!B�B�B��B��B��B�B��B�B�!B�FB�}BÖBĜB��B��B�
B�BB�TB�mB�B�B�B�B�yB�fB�;B�5B�HB�TB�TB�`B�mB�sB�B�yB�mB�sB�B��B		7B	\B	\B	hB	oB	oB	oB	�B	�B	�B	!�B	$�B	%�B	%�B	$�B	#�B	$�B	&�B	+B	0!B	49B	6FB	6FB	8RB	7LB	7LB	8RB	9XB	=qB	C�B	C�B	B�B	@�B	@�B	@�B	C�B	H�B	O�B	R�B	T�B	T�B	YB	^5B	_;B	^5B	[#B	T�B	O�B	Q�B	T�B	T�B	O�B	I�B	B�B	=qB	<jB	B�B	D�B	D�B	C�B	C�B	D�B	D�B	K�B	VB	W
B	XB	YB	[#B	ZB	ZB	ZB	[#B	YB	VB	S�B	R�B	R�B	R�B	R�B	R�B	Q�B	Q�B	R�B	T�B	VB	VB	^5B	bNB	bNB	hsB	m�B	n�B	q�B	q�B	q�B	q�B	r�B	r�B	m�B	e`B	cTB	cTB	`BB	YB	R�B	R�B	cTB	x�B	|�B	�hB	�PB	z�B	o�B	l�B	\)B	S�B	R�B	XB	gmB	]/B	[#B	\)B	_;B	bNB	bNB	m�B	o�B	o�B	n�B	n�B	p�B	u�B	u�B	u�B	u�B	u�B	q�B	q�B	p�B	p�B	p�B	p�B	o�B	o�B	r�B	t�B	v�B	y�B	{�B	�B	�%B	�%B	�1B	�1B	�1B	�7B	�DB	�VB	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�?B	�FB	�LB	�RB	�RB	�^B	�jB	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B
B
B
B
B
1B
	7B
	7B
	7B
	7B

=B
JB
JB
JB
JB
JB
DB
DB

=B
1B
B
B
B
B
B
B
B
B
B
JB
\B
{B
�B
&�B
,B
0!B
7LB
;dB
A�B
F�B
K�B
Q�B
W
B
YB
^5B
cTB
gmB
k�B
p�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	fiB	e`B	d[B	d\B	d[B	eaB	e_B	eaB	e_B	ebB	{�B	��B	�zB	ɸB	��B	��B	�B	�xB
#B
'�B
7CB
S�B
\!B
x�B
�XB
�B
�>B�BYB�xB�B�"B�uBʲB��B�WB�B�B��B:BUBTB.	BB�BU�Bg_BmBijBhgBhgBv�B�B�cB��B�mBʶB�B�SB�$B��B�B�B�B�$B�B��B�jB�B��B��B��B�iB�iB�&B�BV�BO�B
.B�[B��B�:B��B�+B��B�Bu�BS�B,�BlB
��B
� B
�dB
��B
� B
a<B
T�B
N�B
A~B
%�B
dB
B

B
B	��B	��B	�CB	�B	�oB	�!B	r�B	C�B	$�B	�B	[B		4B��B�B�hB�AB�/B�B��BɼBĠB�B�xB�wB�rB�fB�eB�YB�WB�UB�FB�/B� B�B�B��B��B��B�	B��B�B�"B�EB�|BÔBğB��B��B�B�>B�RB�jB�B�B�B�~B�wB�dB�9B�3B�FB�PB�RB�]B�jB�nB�~B�vB�hB�pB�B��B		5B	XB	XB	dB	hB	kB	iB	�B	�B	�B	!�B	$�B	%�B	%�B	$�B	#�B	$�B	&�B	*�B	0B	42B	6=B	6@B	8LB	7DB	7DB	8MB	9OB	=jB	C�B	C�B	B�B	@}B	@{B	@{B	C�B	H�B	O�B	R�B	T�B	T�B	YB	^-B	_1B	^,B	[B	T�B	O�B	Q�B	T�B	T�B	O�B	I�B	B�B	=iB	<_B	B�B	D�B	D�B	C�B	C�B	D�B	D�B	K�B	U�B	WB	X	B	YB	[B	ZB	ZB	ZB	[B	YB	U�B	S�B	R�B	R�B	R�B	R�B	R�B	Q�B	Q�B	R�B	T�B	U�B	U�B	^+B	bDB	bCB	hmB	m�B	n�B	q�B	q�B	q�B	q�B	r�B	r�B	m�B	eWB	cIB	cJB	`7B	YB	R�B	R�B	cKB	x�B	|�B	�]B	�FB	z�B	o�B	l�B	\B	S�B	R�B	XB	gcB	]$B	[B	\ B	_/B	bDB	bDB	m�B	o�B	o�B	n�B	n�B	p�B	u�B	u�B	u�B	u�B	u�B	q�B	q�B	p�B	p�B	p�B	p�B	o�B	o�B	r�B	t�B	v�B	y�B	{�B	�B	�B	�B	�'B	�%B	�%B	�+B	�8B	�JB	�QB	�NB	�ZB	�hB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�%B	�+B	�3B	�8B	�?B	�FB	�GB	�RB	�]B	�mB	�vB	�}B	�{B	�{B	B	ÉB	ÄB	ƙB	ƚB	ǠB	ǡB	ȤB	ɭB	ʴB	ʳB	˹B	˺B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�!B	�&B	�7B	�>B	�FB	�DB	�FB	�LB	�LB	�XB	�fB	�jB	�oB	�uB	�uB	�vB	�wB	�pB	�nB	�{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
B
B
B
!B
	(B
	'B
	'B
	*B

-B
;B
9B
;B
=B
;B
2B
3B

.B
!B
B
B
	B

B
B

B
B
G�O�B
=B
KB
lB
�B
&�B
+�B
0B
7:B
;RB
AxB
F�B
K�B
Q�B
V�B
YB
^$B
cCB
g\B
krB
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.17 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436342016080714363420160807143634  AO  ARCAADJP                                                                    20150401021557    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150401021557  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150401021557  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143634  IP                  G�O�G�O�G�O�                