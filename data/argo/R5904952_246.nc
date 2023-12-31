CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:00Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190600  20181005190600  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @���"6�1   @���-��@1��S���c��`A�71   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@�  @�  A   A!��AA��Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BG��BP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�33B�  B���B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB  CD  CF  CH  CJ  CK�fCM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��3C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C��C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  D   D �fDfD�fDfD� D  Dy�D��DfD�fDfD� D��Dy�D��D� D��Dy�D  D� D��D� D  D� D��D� DfD�fD  Dy�D  D�fD  D� D  Dy�D��D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$fD$�fD%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.fD.� D/  D/�fD0fD0�fD1  D1y�D2  D2� D3  D3�fD4fD4�fD5fD5� D6  D6�fD7  D7� D8  D8y�D9  D9� D:  D:� D;  D;y�D;��D<� D=  D=� D>fD>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DIy�DI��DJy�DK  DK�fDLfDL� DM  DM� DNfDN�fDO  DOy�DP  DP� DQ  DQ� DR  DR� DR��DSy�DS��DTy�DU  DU� DV  DV� DW  DWy�DW��DX� DYfDY� DY��DZ� D[fD[�fD\  D\� D]  D]� D^  D^� D_  D_� D_��D`y�D`��Da� Db  Db�fDc  Dcy�Dc��Dd� De  De� DffDf� Df��Dgy�Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dmy�Dn  Dny�Do  Do�fDp  Dpy�Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Du��Dv� Dw  Dwy�Dw��Dy�{D�?\D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@E�@�@�A�HA$z�ADz�Adz�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@Q�BHQ�BP�RBX�RB`�RBh�RBp�RBy�B��\B�\)B�\)B��\B�\)B�(�B�\)B��\B��\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .C.C.C.C.C
.C.C.C.C.C.C.C.C.CG�C.C .C".C$.C&.C(.C*G�C,G�C..C0.C2.C4.C6.C8.C:zC<.C>.C@.CB.CD.CF.CH.CJ.CLzCNzCP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
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
C�#�C�
C�
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
C�
=C�
C�#�C�
C�
C�
C�
C�
=C�
=C�
C�#�C�
C�
C�
C�
C�
C�
C�
=C�
C�#�C�
C�
C�
=C�
C�
C�
C�#�C�#�C�#�C�
C�
C�
C�
C�
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
C�
=C�
=C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�#�C�#�C�
C�
C�
C�
C�
C�
C�#�C�#�C�#�C�
C�
=C�
C�
C�#�C�#�C�
C�
C�
C�
C�#�C�
C�
=C�
C�
C�
C�
C�
C�
=C�
C�#�C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
D �D ��D�D��D�D��D�D�DD�D��D�D��DD�DD��DD�D�D��DD��D�D��DD��D�D��D�D�D�D��D�D��D�D�DD��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*�D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1�D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8�D9�D9��D:�D:��D;�D;�D<D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI�DJDJ�DK�DK��DL�DL��DM�DM��DN�DN��DO�DO�DP�DP��DQ�DQ��DR�DR��DSDS�DTDT�DU�DU��DV�DV��DW�DW�DXDX��DY�DY��DZDZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`D`�DaDa��Db�Db��Dc�Dc�DdDd��De�De��Df�Df��DgDg�Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm�Dn�Dn�Do�Do��Dp�Dp�Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��DvDv��Dw�Dw�Dw�RDy� D�ED��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�bNA�`BA�\)A�\)A�^5A�`BA�S�A�t�A���A��
A�~�A�+A��A�^5AƗ�A�hsA�ZA�ZA�O�A�?}A�1Ař�A�XA�1A���A���Aħ�Aě�Aė�Aĉ7A�z�A�~�A�n�A�S�A�ZAĕ�A�(�A�t�A�M�A��A��#Aĺ^A�A���AľwA�t�A�M�A�JA�\)A�$�A�oA��A¼jA���A��TA�?}A���A�
=A�l�A��A��RA�ZA�S�A��+A�G�A� �A�C�A���A���A��A�jA�~�A��A��PA� �A�{A��hA�x�A�"�A��A��^A��mA�K�A��A�A�  A�hsA�S�A�r�A�|�A��A�?}A�ĜA�{A�(�A��A��RA�VA�G�A�+A�AO�A|�Azz�AxffAup�Ar��Ap��Ak�Ag�Ad1'Aa�hA[O�AY�AWhsAU��AQƨAN�AKoAF-ADI�AChsAB��AAx�A@�yA@v�A>�\A;�wA8ȴA5�TA1&�A/�^A/�A.{A-ƨA-��A-�#A,��A+%A*�yA*�`A*ĜA)�wA(=qA%t�A%"�A%33A%C�A%VA%
=A$ĜA$=qA$A&  A$�Ap�AffA�hA��A�jA�-AO�AbNA��AE�A�DA�`A�PAȴA�AVA�mA�A�Ap�AXAffA�FAl�A/A/AdZAXA
��A
��A
bNA
M�A
-A	��A��A^5A�A��AQ�A�A �\@��@��@��^@��w@�I�@�\@�-@�&�@���@�9X@�
=@��T@�j@�l�@�-@�X@�bN@�@�\@�-@�`B@�@�@�r�@�A�@�R@�X@���@���@���@ޟ�@�n�@��#@�&�@۾w@�
=@��@ّh@���@ج@�bN@�z�@�I�@��@� �@�\)@җ�@��@�/@���@�Z@�(�@��;@ϥ�@υ@�C�@���@Ο�@��@���@�b@˅@�
=@�ȴ@�J@��@�V@�=q@���@���@�x�@�7L@���@�Ĝ@��@���@�1'@���@�\)@�n�@���@���@�G�@��`@���@��u@�z�@��
@�"�@��R@��@��T@�X@���@��@���@��@���@��;@��
@�ƨ@���@�;d@�33@���@�^5@�J@��h@��@�`B@�O�@�G�@��@�A�@�Z@��@�Ĝ@�7L@��@�M�@���@�@�/@��@���@�z�@��@�Ĝ@�  @���@���@��@�K�@�C�@�C�@��@��
@�1@���@�|�@���@��@�-@��@���@��/@��@�7L@�O�@�hs@�x�@�X@�X@�&�@���@���@�\)@���@�5?@��T@�/@��@���@���@���@�ff@�-@��@���@��7@�p�@�/@���@���@�r�@�1'@��;@�+@���@��\@�~�@�n�@�{@�p�@��@���@���@�%@���@��@�bN@� �@��@�S�@�@�@���@��@��!@�^5@�-@��T@��^@��h@�`B@�X@�/@�V@��@���@��9@��@�Q�@�Q�@�Q�@�A�@� �@�  @��;@�dZ@�K�@��@��@���@�V@�=q@���@��-@��7@���@���@�Q�@�b@���@�"�@���@���@�v�@�5?@��#@��-@��@��@�x�@�G�@�V@�%@���@��@��@�l�@�K�@��y@��@��@��R@��!@��R@��y@��@���@�n�@�ff@�=q@�J@��-@�/@�Ĝ@��@��u@�Ĝ@��/@��@�1'@��m@��P@��?@��@o��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bNA�bNA�`BA�\)A�\)A�^5A�`BA�S�A�t�A���A��
A�~�A�+A��A�^5AƗ�A�hsA�ZA�ZA�O�A�?}A�1Ař�A�XA�1A���A���Aħ�Aě�Aė�Aĉ7A�z�A�~�A�n�A�S�A�ZAĕ�A�(�A�t�A�M�A��A��#Aĺ^A�A���AľwA�t�A�M�A�JA�\)A�$�A�oA��A¼jA���A��TA�?}A���A�
=A�l�A��A��RA�ZA�S�A��+A�G�A� �A�C�A���A���A��A�jA�~�A��A��PA� �A�{A��hA�x�A�"�A��A��^A��mA�K�A��A�A�  A�hsA�S�A�r�A�|�A��A�?}A�ĜA�{A�(�A��A��RA�VA�G�A�+A�AO�A|�Azz�AxffAup�Ar��Ap��Ak�Ag�Ad1'Aa�hA[O�AY�AWhsAU��AQƨAN�AKoAF-ADI�AChsAB��AAx�A@�yA@v�A>�\A;�wA8ȴA5�TA1&�A/�^A/�A.{A-ƨA-��A-�#A,��A+%A*�yA*�`A*ĜA)�wA(=qA%t�A%"�A%33A%C�A%VA%
=A$ĜA$=qA$A&  A$�Ap�AffA�hA��A�jA�-AO�AbNA��AE�A�DA�`A�PAȴA�AVA�mA�A�Ap�AXAffA�FAl�A/A/AdZAXA
��A
��A
bNA
M�A
-A	��A��A^5A�A��AQ�A�A �\@��@��@��^@��w@�I�@�\@�-@�&�@���@�9X@�
=@��T@�j@�l�@�-@�X@�bN@�@�\@�-@�`B@�@�@�r�@�A�@�R@�X@���@���@���@ޟ�@�n�@��#@�&�@۾w@�
=@��@ّh@���@ج@�bN@�z�@�I�@��@� �@�\)@җ�@��@�/@���@�Z@�(�@��;@ϥ�@υ@�C�@���@Ο�@��@���@�b@˅@�
=@�ȴ@�J@��@�V@�=q@���@���@�x�@�7L@���@�Ĝ@��@���@�1'@���@�\)@�n�@���@���@�G�@��`@���@��u@�z�@��
@�"�@��R@��@��T@�X@���@��@���@��@���@��;@��
@�ƨ@���@�;d@�33@���@�^5@�J@��h@��@�`B@�O�@�G�@��@�A�@�Z@��@�Ĝ@�7L@��@�M�@���@�@�/@��@���@�z�@��@�Ĝ@�  @���@���@��@�K�@�C�@�C�@��@��
@�1@���@�|�@���@��@�-@��@���@��/@��@�7L@�O�@�hs@�x�@�X@�X@�&�@���@���@�\)@���@�5?@��T@�/@��@���@���@���@�ff@�-@��@���@��7@�p�@�/@���@���@�r�@�1'@��;@�+@���@��\@�~�@�n�@�{@�p�@��@���@���@�%@���@��@�bN@� �@��@�S�@�@�@���@��@��!@�^5@�-@��T@��^@��h@�`B@�X@�/@�V@��@���@��9@��@�Q�@�Q�@�Q�@�A�@� �@�  @��;@�dZ@�K�@��@��@���@�V@�=q@���@��-@��7@���@���@�Q�@�b@���@�"�@���@���@�v�@�5?@��#@��-@��@��@�x�@�G�@�V@�%@���@��@��@�l�@�K�@��y@��@��@��R@��!@��R@��y@��@���@�n�@�ff@�=q@�J@��-@�/@�Ĝ@��@��u@�Ĝ@��/@��@�1'@��m@��P@��?@��@o��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BVB\B\B\B\BVBVBVB"�B0!BA�Bz�B��B��B�qB��B�
B�NB�B�B�B�B�B�B�B��B	B	B	B	B	B	PB	�B	-B	)�B	33B	M�B	�+B	�3B	��B	�B

=B
"�B
+B
9XB
C�B
ZB
cTB
u�B
��B
�B
�!B
�?B
��B  BuB+B>wBI�B]/Bu�B�{B��B�{B��B��B�fB�B��B��B��B��B��B��B�B�B�mBȴB�-B��B�{B�Bv�B`BBI�B?}B9XB1'B{B1B
�mB
�
B
��B
��B
�\B
� B
jB
F�B
6FB
"�B	�B	�RB	y�B	^5B	P�B	B�B	2-B	 �B	oB�B�NB��BɺBBɺB�HB��B	B	B��B�B�B�B�mB�ZB�ZB�fB��B��B�B�#B�qB�?B�'B�-B�FBB��B��B�B�)B�)B�#B�B��B��B�/B�sB�B��B		7B	JB	DB	�B	N�B	I�B	�B	
=B	DB�B�fB�mB�B	B	�B	!�B	'�B	5?B	H�B	J�B	H�B	B�B	:^B	1'B	#�B	uB		7B	%B��B��B��B��B	B	
=B	�B	�B	�B	�B	�B	{B	
=B��B�yB�ZB�ZB�fB�mB�)B�B�HB�BB�)B�)B�/B�BB�NB�mB��B��B	+B	1B	1B		7B	
=B	
=B	
=B		7B	+B	%B	PB	�B	�B	�B	hB	\B	VB	VB	PB	JB	VB	oB	uB	uB	uB	uB	{B	�B	�B	#�B	(�B	,B	&�B	%�B	(�B	-B	.B	49B	;dB	;dB	>wB	A�B	D�B	F�B	I�B	I�B	O�B	S�B	S�B	T�B	VB	VB	YB	\)B	]/B	�B	�%B	�%B	�%B	�+B	�DB	�JB	�JB	�JB	�\B	�bB	�hB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�3B	�FB	�RB	�XB	�^B	�^B	�wB	��B	��B	ÖB	ŢB	ǮB	��B	��B	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�/B	�;B	�;B	�5B	�5B	�5B	�)B	�/B	�HB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
%B
%B
%B
+B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
PB
VB
VB
VB
bB
bB
bB
hB
hB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
*KB
7�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222BVB\B\B\B\BVBVBVB"�B0!BA�Bz�B��B��B�qB��B�
B�NB�B�B�B�B�B�B�B��B	B	B	B	B	B	PB	�B	-B	)�B	33B	M�B	�+B	�3B	��B	�B

=B
"�B
+B
9XB
C�B
ZB
cTB
u�B
��B
�B
�!B
�?B
��B  BuB+B>wBI�B]/Bu�B�{B��B�{B��B��B�fB�B��B��B��B��B��B��B�B�B�mBȴB�-B��B�{B�Bv�B`BBI�B?}B9XB1'B{B1B
�mB
�
B
��B
��B
�\B
� B
jB
F�B
6FB
"�B	�B	�RB	y�B	^5B	P�B	B�B	2-B	 �B	oB�B�NB��BɺBBɺB�HB��B	B	B��B�B�B�B�mB�ZB�ZB�fB��B��B�B�#B�qB�?B�'B�-B�FBB��B��B�B�)B�)B�#B�B��B��B�/B�sB�B��B		7B	JB	DB	�B	N�B	I�B	�B	
=B	DB�B�fB�mB�B	B	�B	!�B	'�B	5?B	H�B	J�B	H�B	B�B	:^B	1'B	#�B	uB		7B	%B��B��B��B��B	B	
=B	�B	�B	�B	�B	�B	{B	
=B��B�yB�ZB�ZB�fB�mB�)B�B�HB�BB�)B�)B�/B�BB�NB�mB��B��B	+B	1B	1B		7B	
=B	
=B	
=B		7B	+B	%B	PB	�B	�B	�B	hB	\B	VB	VB	PB	JB	VB	oB	uB	uB	uB	uB	{B	�B	�B	#�B	(�B	,B	&�B	%�B	(�B	-B	.B	49B	;dB	;dB	>wB	A�B	D�B	F�B	I�B	I�B	O�B	S�B	S�B	T�B	VB	VB	YB	\)B	]/B	�B	�%B	�%B	�%B	�+B	�DB	�JB	�JB	�JB	�\B	�bB	�hB	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�'B	�3B	�FB	�RB	�XB	�^B	�^B	�wB	��B	��B	ÖB	ŢB	ǮB	��B	��B	ĜB	ĜB	ĜB	ŢB	ŢB	ŢB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�/B	�;B	�;B	�5B	�5B	�5B	�)B	�/B	�HB	�ZB	�`B	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
%B
%B
%B
+B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
JB
PB
VB
VB
VB
bB
bB
bB
hB
hB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

B
*KB
7�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190600                              AO  ARCAADJP                                                                    20181005190600    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190600  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190600  QCF$                G�O�G�O�G�O�8000            