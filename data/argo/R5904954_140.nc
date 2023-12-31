CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:20Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005191720  20181005191720  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��d�d�!1   @��eO��@5�����dx9XbN1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�33B���B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C�C  C�fC�fC  C  C  C�C  C�fC   C"  C$  C&  C(  C*  C,  C.  C/�fC1�fC3�fC6  C8  C:  C;�fC=�fC?�fCB  CC�fCE�fCH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`  Ca�fCc�fCf  Ch  Cj  Ck�fCm�fCp�Cr�Ct�Cv  Cx  Cz�C|  C}�fC�  C�  C��3C��C�  C��3C�  C�  C��C�  C��C��C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C��C��C�  C��fC�  C��C�  C�  C��3C�  C�  C��3C��3C�  C��C�  C��3C��C��C�  C��3C��3C��C��C�  C��3C�  C�  C�  C�  C�  C�  C��C��C�  C��3C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C��C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C��C�  C��C��C��C��C�  C��3C��3C��3C�  C��3C��3C�  C��C�  C��3C�  C�  C�  C�  C��C��C��3C��3D y�D ��D� D  D� D  D�fD  D� D��D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D�fD  D� D  D� DfD�fD  D� D  D� DfD� D  D�fD  D� DfD�fD�D�fD  D� D  D� D  D� D��D� DfD� DfD� D  D�fDfD�fD  Dy�D��D� D   D y�D!  D!� D!��D"� D#  D#� D$  D$y�D%  D%y�D&  D&� D'  D'�fD(fD(�fD)  D)y�D*  D*� D+  D+y�D+��D,y�D-fD-� D-��D.�fD/fD/y�D/��D0� D1  D1y�D2  D2� D2��D3� D4  D4y�D5  D5�fD6  D6� D6��D7� D8  D8� D9  D9�fD:  D:� D:��D;� D<fD<�fD=  D=� D>fD>�fD?  D?� D@  D@�fDA  DAy�DA��DB� DC  DCy�DD  DD� DE  DE� DF  DF�fDF��DG� DHfDH�fDH��DIy�DI��DJ� DK  DK� DL  DL� DM  DM� DN  DNy�DN��DO�fDPfDP�fDQ  DQ� DQ��DRy�DS  DS� DT  DTy�DT��DU� DV  DVy�DV��DW� DW��DXy�DY  DY� DZfDZ�fD[  D[�fD\  D\� D]fD]� D^  D^� D_fD_� D`  D`�fD`��Da� Db  Db� Dc  Dc� Dd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Di��Dj� Dk  Dky�Dl  Dl�fDm  Dmy�Dm��Dn� Dn��Do� Dp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDufDu� DvfDv�fDwfDw�fDw�3Dy��D�(�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��H@�G�A ��A ��A@��A`��A�Q�A��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�BB(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�G�B�G�B�G�B��HB�{B�{B�{B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�G�B�{B�{B�{B�{B�{B��HB�{C 
=C
=C
=C
=C
=C

=C#�C
=C�C�C
=C
=C
=C#�C
=C�C 
=C"
=C$
=C&
=C(
=C*
=C,
=C.
=C/�C1�C3�C6
=C8
=C:
=C;�C=�C?�CB
=CC�CE�CH#�CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C\#�C^#�C`
=Ca�Cc�Cf
=Ch
=Cj
=Ck�Cm�Cp#�Cr#�Ct#�Cv
=Cx
=Cz#�C|
=C}�C�C�C��RC��C�C��RC�C�C��C�C��C��C�C�C�C�C�C�C��C��C��C��C�C�C��C��C�C��C�C��C�C�C��RC�C�C��RC��RC�C��C�C��RC��C��C�C��RC��RC��C��C�C��RC�C�C�C�C�C�C��C��C�C��RC�C��C��C��C��C�C�C�C�C�C�C��RC��RC��RC��RC�C��C�C�C�C��RC��RC�C�C��RC�C�C�C�C�C��C�C�C�C�C�C�C��C�C�C��RC�C�C�C��C�C��C��C��C��C�C��RC��RC��RC�C��RC��RC�C��C�C��RC�C�C�C�C��C��C��RC��RD |)D �)D��D�D��D�D��D�D��D�)D��D�D��D�D��D�D��D	�D	��D
�D
|)D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D\D��D�D��D�D��D�D��D�)D��D�D��D�D��D�D��D�D��D�D|)D�)D��D �D |)D!�D!��D!�)D"��D#�D#��D$�D$|)D%�D%|)D&�D&��D'�D'��D(�D(��D)�D)|)D*�D*��D+�D+|)D+�)D,|)D-�D-��D-�)D.��D/�D/|)D/�)D0��D1�D1|)D2�D2��D2�)D3��D4�D4|)D5�D5��D6�D6��D6�)D7��D8�D8��D9�D9��D:�D:��D:�)D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA|)DA�)DB��DC�DC|)DD�DD��DE�DE��DF�DF��DF�)DG��DH�DH��DH�)DI|)DI�)DJ��DK�DK��DL�DL��DM�DM��DN�DN|)DN�)DO��DP�DP��DQ�DQ��DQ�)DR|)DS�DS��DT�DT|)DT�)DU��DV�DV|)DV�)DW��DW�)DX|)DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��D`�)Da��Db�Db��Dc�Dc��Dd�Dd|)De�De��Df�Df��Dg�Dg��Dh�Dh|)Di�Di��Di�)Dj��Dk�Dk|)Dl�Dl��Dm�Dm|)Dm�)Dn��Dn�)Do��Dp�Dp|)Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw��Dy��D�*>11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�33A�33A�1'A�33A�1'A�/A�+A�$�A�1'A�7LA�7LA�9XA�9XA�=qA�7LA�5?A�5?A�5?A�$�A�|�A�VA�$�AуAЋDAϝ�A���A���A��HA�M�A�ȴA�A��A��A�9XAĲ-A+A�|�A��A��A�\)A��7A�bNA�(�A�|�A�|�A�33A���A���A��yA�ȴA�|�A�Q�A�n�A���A�
=A��A�hsA�-A�Q�A��A��A���A�  A�bA�1A���A���A��7A�&�A�^5A��HA�C�A��A�+A���A��wA�bNA��;A��A�~�A�hsA�=qA�ĜA�
=A�ȴA�n�A�VA�A~��A{�AzbAx~�Aw�Av�uAu�AtĜAs��ApbAoVAnJAl�\AiK�Ag&�Ad �AcC�Abz�A`�+A]S�A\-A[AY%AVȴAV{AU�AR�AM\)AJn�AI33AG�AF�AD  AA�A?�7A>5?A<��A:�`A: �A9��A7�TA4��A2�yA1�PA0��A0z�A/��A.ĜA-t�A,�A+�A*��A)�A);dA'�hA&1'A%�7A%&�A$�RA$�+A$r�A#A"JA �yA�A|�AVA�A��AoA�A9XA�-A�A�wA1'AdZA�A�DAƨA
��A	��A�9A��AC�A��A��A��A�A`BA"�A�A �A+A��AE�A�mAdZA ~�@���@��7@�r�@�K�@��/@�v�@��/@���@�u@�M�@�x�@�@�@�!@��@�x�@�O�@�1'@�\@�p�@�A�@��@��@�j@�;d@�G�@�(�@�K�@�M�@���@�A�@�C�@�O�@�;d@ҏ\@�`B@�A�@�$�@�b@�
=@�`B@�z�@�(�@Ǿw@�o@�V@��/@�@\@���@��H@�-@��@�Ĝ@�b@�o@��@��/@��m@�n�@��@��@�{@���@��h@�p�@�?}@�%@��@��@�G�@�%@��/@��9@���@��@�z�@� �@��;@��h@�^5@�@���@�(�@�\)@��@���@���@�~�@��@�1@�l�@�;d@�o@�
=@��@�\)@�33@�@�ȴ@�-@�7L@�hs@�\)@�K�@���@�S�@�~�@��h@�%@�Ĝ@���@�z�@�bN@��@��@��D@�`B@�&�@�r�@��@���@���@�ƨ@��P@���@��\@�=q@�J@��#@���@�`B@�O�@�O�@�O�@�O�@�7L@�&�@�%@���@���@��@���@���@��+@�ff@�=q@�J@��^@�@��-@��h@��@��7@�x�@��-@��-@�`B@�hs@���@��j@�A�@��@��@���@��@���@��@�t�@�"�@��!@���@�V@��@�=q@�{@�5?@���@���@��!@�5?@���@��u@�bN@�bN@��F@��@��
@�dZ@��+@��#@�`B@�/@��D@�1@�dZ@�\)@�+@�@���@�{@��@���@�J@�=q@��+@�n�@��7@�Q�@��j@�&�@�hs@�O�@��@�x�@���@��D@�j@�(�@���@��@�t�@��@��@��H@��@�1'@�z�@���@��@���@��`@���@���@�bN@��
@�l�@�l�@�
=@�V@��h@��/@�|�@�E�@�ff@�E�@�o@�"�@���@��R@�ȴ@�J@�z�@�(�@�r�@�S�@�"�@���@�Q�@�A�@��D@��`@��`@���@�r�@��m@���@��F@��@�dZ@�"�@��@���@��#@�O�@��`@�Z@�9X@��@�;@K�@
=@~$�@~{@~@}��@}@}�@}O�@}?}@}�@|�@|(�@{dZ@{o@z�H@zn�@z=q@z�@y��@y��@zJ@x��@x��@x�`@x��@x��@y�@ym]@i7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�33A�33A�1'A�33A�1'A�/A�+A�$�A�1'A�7LA�7LA�9XA�9XA�=qA�7LA�5?A�5?A�5?A�$�A�|�A�VA�$�AуAЋDAϝ�A���A���A��HA�M�A�ȴA�A��A��A�9XAĲ-A+A�|�A��A��A�\)A��7A�bNA�(�A�|�A�|�A�33A���A���A��yA�ȴA�|�A�Q�A�n�A���A�
=A��A�hsA�-A�Q�A��A��A���A�  A�bA�1A���A���A��7A�&�A�^5A��HA�C�A��A�+A���A��wA�bNA��;A��A�~�A�hsA�=qA�ĜA�
=A�ȴA�n�A�VA�A~��A{�AzbAx~�Aw�Av�uAu�AtĜAs��ApbAoVAnJAl�\AiK�Ag&�Ad �AcC�Abz�A`�+A]S�A\-A[AY%AVȴAV{AU�AR�AM\)AJn�AI33AG�AF�AD  AA�A?�7A>5?A<��A:�`A: �A9��A7�TA4��A2�yA1�PA0��A0z�A/��A.ĜA-t�A,�A+�A*��A)�A);dA'�hA&1'A%�7A%&�A$�RA$�+A$r�A#A"JA �yA�A|�AVA�A��AoA�A9XA�-A�A�wA1'AdZA�A�DAƨA
��A	��A�9A��AC�A��A��A��A�A`BA"�A�A �A+A��AE�A�mAdZA ~�@���@��7@�r�@�K�@��/@�v�@��/@���@�u@�M�@�x�@�@�@�!@��@�x�@�O�@�1'@�\@�p�@�A�@��@��@�j@�;d@�G�@�(�@�K�@�M�@���@�A�@�C�@�O�@�;d@ҏ\@�`B@�A�@�$�@�b@�
=@�`B@�z�@�(�@Ǿw@�o@�V@��/@�@\@���@��H@�-@��@�Ĝ@�b@�o@��@��/@��m@�n�@��@��@�{@���@��h@�p�@�?}@�%@��@��@�G�@�%@��/@��9@���@��@�z�@� �@��;@��h@�^5@�@���@�(�@�\)@��@���@���@�~�@��@�1@�l�@�;d@�o@�
=@��@�\)@�33@�@�ȴ@�-@�7L@�hs@�\)@�K�@���@�S�@�~�@��h@�%@�Ĝ@���@�z�@�bN@��@��@��D@�`B@�&�@�r�@��@���@���@�ƨ@��P@���@��\@�=q@�J@��#@���@�`B@�O�@�O�@�O�@�O�@�7L@�&�@�%@���@���@��@���@���@��+@�ff@�=q@�J@��^@�@��-@��h@��@��7@�x�@��-@��-@�`B@�hs@���@��j@�A�@��@��@���@��@���@��@�t�@�"�@��!@���@�V@��@�=q@�{@�5?@���@���@��!@�5?@���@��u@�bN@�bN@��F@��@��
@�dZ@��+@��#@�`B@�/@��D@�1@�dZ@�\)@�+@�@���@�{@��@���@�J@�=q@��+@�n�@��7@�Q�@��j@�&�@�hs@�O�@��@�x�@���@��D@�j@�(�@���@��@�t�@��@��@��H@��@�1'@�z�@���@��@���@��`@���@���@�bN@��
@�l�@�l�@�
=@�V@��h@��/@�|�@�E�@�ff@�E�@�o@�"�@���@��R@�ȴ@�J@�z�@�(�@�r�@�S�@�"�@���@�Q�@�A�@��D@��`@��`@���@�r�@��m@���@��F@��@�dZ@�"�@��@���@��#@�O�@��`@�Z@�9X@��@�;@K�@
=@~$�@~{@~@}��@}@}�@}O�@}?}@}�@|�@|(�@{dZ@{o@z�H@zn�@z=q@z�@y��@y��@zJ@x��@x��@x�`@x��@x��@y�@ym]@i7L11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B�B�B{BuB�B#�B0!BA�BE�BJ�BM�BR�BW
BYBgmBr�Bz�B�B�B��B�9B��BȴB��B��BȴB��B��B��B�B�B��BȴB�B�VB�B�Bm�BcTBcTBgmBVB1'BuB	7BB�mB��BB�9B��B��B�%Br�BgmB`BBVBG�B8RB0!B!�B\B
��B
�B
�ZB
��B
ÖB
�B
��B
�B
{�B
k�B
N�B
9XB
$�B
�B
bB
+B
B	��B	��B	�B	�B	��B	ŢB	�^B	��B	��B	�DB	�B	}�B	r�B	bNB	[#B	W
B	L�B	?}B	:^B	33B	"�B	PB	  B��B��B�sB�ZB�/B��B��B��BŢBB�}B�XB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�JB�Bz�Bw�Bq�BjBbNB_;B]/B[#BXBT�BQ�BN�BL�BJ�BI�BH�BG�BF�BD�BC�BB�BA�B?}B>wB=qB<jB;dB9XB7LB6FB6FB49B33B2-B0!B0!B.B.B.B-B-B-B-B,B,B+B+B,B,B+B,B,B+B,B,B-B.B/B.B.B/B0!B1'B6FB6FB49B49B49B5?B8RB<jB=qBB�BE�BJ�BP�BS�BZBm�Bo�Bn�Bp�Bq�Bq�Bn�Bn�Bn�Bo�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bo�Bq�Bs�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bu�Bv�Bw�Br�Bp�Bq�Bt�Bw�Bx�By�Bz�Bz�B{�B�B�7B�VB�oB��B��B�B�B�B�B�9B�?B�XB�LB�jB�}B�}B�qB�^B�dB�qB�}B��BĜB��B��B��B�B�fB�B��B��B��B��B	  B	%B	
=B	VB	oB	{B	�B	 �B	$�B	%�B	&�B	,B	1'B	2-B	5?B	8RB	9XB	9XB	8RB	9XB	9XB	;dB	=qB	B�B	G�B	H�B	K�B	O�B	Q�B	S�B	T�B	W
B	XB	W
B	YB	YB	[#B	^5B	_;B	`BB	gmB	iyB	jB	k�B	k�B	m�B	o�B	p�B	q�B	s�B	x�B	|�B	~�B	�B	�%B	�1B	�DB	�JB	�=B	�7B	�7B	�+B	�=B	�\B	�oB	�hB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�'B	�-B	�-B	�3B	�9B	�?B	�dB	��B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	��B	��B	��B	�
B	�B	�B	�B	�
B	��B	��B	�B	��B	��B	�B	�HB	�TB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B{B{B{B{B{B{B{B{B{B{B{B{B{B{B{B�B�B{BuB�B#�B0!BA�BE�BJ�BM�BR�BW
BYBgmBr�Bz�B�B�B��B�9B��BȴB��B��BȴB��B��B��B�B�B��BȴB�B�VB�B�Bm�BcTBcTBgmBVB1'BuB	7BB�mB��BB�9B��B��B�%Br�BgmB`BBVBG�B8RB0!B!�B\B
��B
�B
�ZB
��B
ÖB
�B
��B
�B
{�B
k�B
N�B
9XB
$�B
�B
bB
+B
B	��B	��B	�B	�B	��B	ŢB	�^B	��B	��B	�DB	�B	}�B	r�B	bNB	[#B	W
B	L�B	?}B	:^B	33B	"�B	PB	  B��B��B�sB�ZB�/B��B��B��BŢBB�}B�XB�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�JB�Bz�Bw�Bq�BjBbNB_;B]/B[#BXBT�BQ�BN�BL�BJ�BI�BH�BG�BF�BD�BC�BB�BA�B?}B>wB=qB<jB;dB9XB7LB6FB6FB49B33B2-B0!B0!B.B.B.B-B-B-B-B,B,B+B+B,B,B+B,B,B+B,B,B-B.B/B.B.B/B0!B1'B6FB6FB49B49B49B5?B8RB<jB=qBB�BE�BJ�BP�BS�BZBm�Bo�Bn�Bp�Bq�Bq�Bn�Bn�Bn�Bo�Bp�Bp�Bp�Bp�Bp�Bo�Bo�Bo�Bq�Bs�Bs�Bs�Bt�Bt�Bt�Bt�Bt�Bu�Bv�Bw�Br�Bp�Bq�Bt�Bw�Bx�By�Bz�Bz�B{�B�B�7B�VB�oB��B��B�B�B�B�B�9B�?B�XB�LB�jB�}B�}B�qB�^B�dB�qB�}B��BĜB��B��B��B�B�fB�B��B��B��B��B	  B	%B	
=B	VB	oB	{B	�B	 �B	$�B	%�B	&�B	,B	1'B	2-B	5?B	8RB	9XB	9XB	8RB	9XB	9XB	;dB	=qB	B�B	G�B	H�B	K�B	O�B	Q�B	S�B	T�B	W
B	XB	W
B	YB	YB	[#B	^5B	_;B	`BB	gmB	iyB	jB	k�B	k�B	m�B	o�B	p�B	q�B	s�B	x�B	|�B	~�B	�B	�%B	�1B	�DB	�JB	�=B	�7B	�7B	�+B	�=B	�\B	�oB	�hB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�'B	�'B	�-B	�-B	�3B	�9B	�?B	�dB	��B	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	��B	��B	��B	�
B	�B	�B	�B	�
B	��B	��B	�B	��B	��B	�B	�HB	�TB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191720                              AO  ARCAADJP                                                                    20181005191720    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191720  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191720  QCF$                G�O�G�O�G�O�8000            