CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-02T20:18:06Z AOML 3.0 creation; 2016-08-07T21:51:26Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160202201806  20160807145126  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               fA   AO  5287_9017_102                   2C  D   APEX                            6529                            072314                          846 @ג����1   @ג�>��C@1/\(��d�hr� �1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    fA   B   B   @333@�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D��D�FfD�� D�ٚD�fD�@ D��fD�� D�� D�0 D��fD��fD���D�I�D�|�D��3D�3D�C3D��D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @N�R@�@�A�HA&�HAF�HAhz�A�p�A�p�A�p�A�p�A�p�A�p�A�p�A��B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B��)B��)B��)B��)BԨ�B��)B��)B��)B��)B�\B�\B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�*=C�*=C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�D��D�T)D���D��]D�)D�M�D��)D���D���D�=�D��)D��)D�]D�W]Dڊ�D���D� �D�P�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�$�A�(�A�&�A�33A�/A�=qA�=qA�=qA�7LA�=qA�=qA�M�A�S�A�hsA�l�A�l�A�p�A�t�A·+AΏ\AΑhAΑhA΍PA΍PA΍PAΏ\AΑhA΋DA΋DA΋DAΉ7A·+A·+A·+A·+AΉ7A΋DAΛ�Aδ9A��#A���A�  A���A�  A�1A�A�1A�"�A�+A�$�A�A�ȴA�|�A�S�A̺^A��A�p�A�&�A�  Aʗ�A���AčPA�\)A���A�r�A���A���A�C�A�5?A���A�ȴA�I�A�  A���A�oA�|�A�ffA�|�A��^A��;A�(�A���A���A�1A�-A���A��A�XA�5?A�1'A��PA��A��PA�dZA���A��`A��A�ȴA� �A��TA��yA�9XA��\A�ffA��`A��A��DA��A��uA�;dA�&�A�-A~  Az��As�An�+Ai
=Ac?}A_�A^�A[AV��AU�AT�RAO+AJ�yAJ �AH��AHZAD��AB�`AA�;A>A�A<5?A:�HA:5?A8��A6n�A3�-A/��A-�A-7LA,E�A(^5A%��A#&�A!"�A �jA�#A�!A��A�HA�#A=qA��AK�A�-AhsAjA�A/A�jAI�A �A�A1A7LAoA��A+A?}AG�A��A��A��A�uA��A�A��A�hA+Az�A1AS�A��A&�AJAhsA
��A
A�A
�A
��A	�hA�DA1'A{A��A�^Al�AC�A+A�jA��A�A33A��A��A1'A;dA;dAK�AdZA��Ap�A �/A z�@�l�@�K�@�K�@���@��@�n�@��/@��m@�"�@�Z@�  @�5?@�&�@��D@�dZ@�@��@�1@�@��@�@�  @�I�@�j@�/@��@���@��@��`@��`@�Ĝ@�1@�S�@�5?@�I�@�J@�O�@�O�@��D@�I�@��@�o@�J@�`B@�ƨ@�^5@�V@ّh@�/@��/@׮@�
=@���@��@�dZ@�V@Ѳ-@мj@�j@�9X@��
@�|�@��y@Ώ\@�=q@�hs@��@̃@��m@�ƨ@�S�@���@ʇ+@�E�@���@�O�@�b@�1@�1@���@��H@�-@��@�x�@ģ�@�(�@ÍP@�K�@���@§�@�v�@�E�@��^@��7@�p�@�`B@�V@���@���@�Z@�t�@���@�^5@���@�V@���@��u@�Q�@�A�@� �@��@��
@�l�@��H@���@�~�@�v�@�E�@�@�7L@��/@��u@�bN@�Q�@�9X@��m@�;d@�+@�;d@�;d@�"�@�ȴ@�-@��#@��h@�`B@�G�@��@��j@�A�@�dZ@�o@�@��y@���@���@��\@�ff@�M�@�5?@�@��T@�7L@�Z@���@��@���@���@��P@�S�@�"�@���@�^5@���@��@���@�?}@�%@��/@�z�@� �@���@�S�@��@���@�M�@�$�@�@�@��h@��/@�Q�@���@��@��R@���@���@�@�r�@�|�@�K�@�33@��H@���@�ff@�E�@�-@�@��T@�@�x�@�?}@�V@���@��@�\)@�C�@�+@�
=@�v�@��^@�x�@�`B@�X@�O�@��@��@���@���@���@�ƨ@���@�t�@�K�@�+@���@��+@�M�@���@��^@�V@��u@�1'@��;@���@��@�E�@���@�x�@�O�@�V@���@��j@���@��D@� �@�t�@�S�@���@�-@���@�@���@��7@�X@�&�@�&�@��@�&�@�%@�z�@�9X@��m@���@��P@�dZ@�33@�"�@��@�=q@���@�p�@�7L@�|�@��@zn�@q�^@f�y@^5?@V��@N5?@G\)@@bN@;��@6E�@0��@*�H@'��@!&�@1@�P@�j@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�$�A�(�A�&�A�33A�/A�=qA�=qA�=qA�7LA�=qA�=qA�M�A�S�A�hsA�l�A�l�A�p�A�t�A·+AΏ\AΑhAΑhA΍PA΍PA΍PAΏ\AΑhA΋DA΋DA΋DAΉ7A·+A·+A·+A·+AΉ7A΋DAΛ�Aδ9A��#A���A�  A���A�  A�1A�A�1A�"�A�+A�$�A�A�ȴA�|�A�S�A̺^A��A�p�A�&�A�  Aʗ�A���AčPA�\)A���A�r�A���A���A�C�A�5?A���A�ȴA�I�A�  A���A�oA�|�A�ffA�|�A��^A��;A�(�A���A���A�1A�-A���A��A�XA�5?A�1'A��PA��A��PA�dZA���A��`A��A�ȴA� �A��TA��yA�9XA��\A�ffA��`A��A��DA��A��uA�;dA�&�A�-A~  Az��As�An�+Ai
=Ac?}A_�A^�A[AV��AU�AT�RAO+AJ�yAJ �AH��AHZAD��AB�`AA�;A>A�A<5?A:�HA:5?A8��A6n�A3�-A/��A-�A-7LA,E�A(^5A%��A#&�A!"�A �jA�#A�!A��A�HA�#A=qA��AK�A�-AhsAjA�A/A�jAI�A �A�A1A7LAoA��A+A?}AG�A��A��A��A�uA��A�A��A�hA+Az�A1AS�A��A&�AJAhsA
��A
A�A
�A
��A	�hA�DA1'A{A��A�^Al�AC�A+A�jA��A�A33A��A��A1'A;dA;dAK�AdZA��Ap�A �/A z�@�l�@�K�@�K�@���@��@�n�@��/@��m@�"�@�Z@�  @�5?@�&�@��D@�dZ@�@��@�1@�@��@�@�  @�I�@�j@�/@��@���@��@��`@��`@�Ĝ@�1@�S�@�5?@�I�@�J@�O�@�O�@��D@�I�@��@�o@�J@�`B@�ƨ@�^5@�V@ّh@�/@��/@׮@�
=@���@��@�dZ@�V@Ѳ-@мj@�j@�9X@��
@�|�@��y@Ώ\@�=q@�hs@��@̃@��m@�ƨ@�S�@���@ʇ+@�E�@���@�O�@�b@�1@�1@���@��H@�-@��@�x�@ģ�@�(�@ÍP@�K�@���@§�@�v�@�E�@��^@��7@�p�@�`B@�V@���@���@�Z@�t�@���@�^5@���@�V@���@��u@�Q�@�A�@� �@��@��
@�l�@��H@���@�~�@�v�@�E�@�@�7L@��/@��u@�bN@�Q�@�9X@��m@�;d@�+@�;d@�;d@�"�@�ȴ@�-@��#@��h@�`B@�G�@��@��j@�A�@�dZ@�o@�@��y@���@���@��\@�ff@�M�@�5?@�@��T@�7L@�Z@���@��@���@���@��P@�S�@�"�@���@�^5@���@��@���@�?}@�%@��/@�z�@� �@���@�S�@��@���@�M�@�$�@�@�@��h@��/@�Q�@���@��@��R@���@���@�@�r�@�|�@�K�@�33@��H@���@�ff@�E�@�-@�@��T@�@�x�@�?}@�V@���@��@�\)@�C�@�+@�
=@�v�@��^@�x�@�`B@�X@�O�@��@��@���@���@���@�ƨ@���@�t�@�K�@�+@���@��+@�M�@���@��^@�V@��u@�1'@��;@���@��@�E�@���@�x�@�O�@�V@���@��j@���@��D@� �@�t�@�S�@���@�-@���@�@���@��7@�X@�&�@�&�@��@�&�@�%@�z�@�9X@��m@���@��P@�dZ@�33@�"�@��@�=q@���@�p�G�O�@�|�@��@zn�@q�^@f�y@^5?@V��@N5?@G\)@@bN@;��@6E�@0��@*�H@'��@!&�@1@�P@�j@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
#�B
#�B
#�B
"�B
"�B
$�B
$�B
#�B
#�B
#�B
#�B
'�B
+B
5?B
6FB
7LB
9XB
:^B
D�B
K�B
K�B
L�B
L�B
K�B
K�B
L�B
M�B
M�B
L�B
M�B
L�B
M�B
N�B
O�B
O�B
O�B
P�B
XB
bNB
s�B
�B
�bB
��B
��B
��B
��B
�B
�XB
ÖB
�
B
�mB
�BuB]/Bt�B�3B�TB�B�B��B6FB^5By�B� B�B�B�\B��B�B�dBÖBÖBB�}B�^B�'B��B��B�oB�PB�B|�Bw�Bw�Bt�Bq�Bx�B�B��B��B��B��B�VB� BgmB>wB�B��B�/B�XB�uBaHBA�B&�B
��B
��B
�\B
�B
O�B
!�B	��B	�B	�mB
B	��B	�'B	�\B	m�B	YB	M�B	7LB	�B	�B	VB��B�ZB�HB�5B�#B�B��BɺB��B�qB�dB�XB�LB�FB�9B�9B�-B�-B�!B�!B�'B�B�'B�'B�9B�qB��B�
B�5B�B�HB�B�BĜB�jB��B��B��B��B��B��B��B��B�NB�BB�/B�BB�TB�sB��B	uB	1'B	W
B	bNB	`BB	[#B	YB	VB	Q�B	K�B	G�B	?}B	:^B	9XB	6FB	6FB	?}B	F�B	B�B	<jB	=qB	@�B	@�B	G�B	K�B	K�B	K�B	M�B	H�B	@�B	A�B	B�B	I�B	L�B	R�B	R�B	VB	ZB	aHB	dZB	e`B	m�B	z�B	�B	�B	�B	}�B	v�B	o�B	iyB	e`B	aHB	ffB	bNB	^5B	\)B	\)B	bNB	ffB	hsB	ffB	bNB	bNB	jB	r�B	{�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	}�B	v�B	n�B	n�B	p�B	o�B	o�B	n�B	n�B	n�B	o�B	o�B	n�B	p�B	o�B	q�B	s�B	w�B	x�B	{�B	|�B	{�B	}�B	� B	�B	�B	�%B	�1B	�7B	�PB	�bB	�bB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�9B	�?B	�RB	�XB	�XB	�XB	�^B	�dB	�dB	�jB	�qB	�wB	�jB	�dB	�^B	�XB	�XB	�^B	�dB	�dB	�dB	�dB	�dB	�dB	�jB	�jB	�jB	�wB	�}B	��B	B	ĜB	ĜB	ĜB	ĜB	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�)B	�#B	�)B	�)B	�)B	�/B	�5B	�BB	�BB	�BB	�NB	�ZB	�fB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
	7B

=B

=B
DB
DB
JB
PB
VB
\B
\B
\B
bB
bB
bB
bB
hB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
)�B
7LB
<jB
?}B
E�B
J�B
O�B
W
B
[#B
^5B
bNB
dZB
jB
n�B
s�B
w�B
{�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
#�B
#�B
#�B
"�B
"�B
$�B
$�B
#�B
#�B
#�B
#�B
'�B
*�B
5#B
6+B
71B
9<B
:DB
DB
K�B
K�B
L�B
L�B
K�B
K�B
L�B
M�B
M�B
L�B
M�B
L�B
M�B
N�B
O�B
O�B
O�B
P�B
W�B
b3B
s�B
��B
�AB
�{B
��B
��B
��B
��B
�;B
�tB
��B
�NB
�BTB]Bt�B�B�1B�tB�B��B6&B^By�B�B��B��B�7B��B��B�=B�nB�sB�mB�VB�9B�B��B�iB�KB�+B��B|�Bw�Bw�Bt�Bq�Bx�B��B�hB��B��B�|B�1B�BgHB>OB�B��B�B�.B�NBaBA^B&�B
ˠB
��B
�8B
��B
O�B
!�B	��B	�jB	�IB
 �B	��B	�B	�;B	mqB	X�B	M�B	7-B	�B	cB	7B��B�>B�,B�B�B��BλBɞB�nB�UB�LB�:B�-B�*B�B�B�B�B�B�B�
B��B�	B�B�B�RB̬B��B�B��B�$B�tB��B�}B�KB�gB̭B��B��B��B��B��B��B�-B�!B�B�!B�3B�RB��B	RB	1 B	V�B	b'B	`B	Z�B	X�B	U�B	Q�B	K�B	G�B	?VB	:8B	91B	6 B	6B	?WB	F�B	BgB	<CB	=JB	@[B	@\B	G�B	K�B	K�B	K�B	M�B	H�B	@^B	AbB	BgB	I�B	L�B	R�B	R�B	U�B	Y�B	a B	d4B	e8B	mkB	z�B	��B	��B	��B	}�B	v�B	osB	iQB	e7B	aB	f;B	b%B	^B	\ B	[�B	b#B	f;B	hLB	f;B	b%B	b#B	jWB	r�B	{�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	}�B	v�B	nnB	nkB	pzB	osB	orB	nnB	nlB	nlB	ouB	osB	nmB	pxB	orB	qB	s�B	w�B	x�B	{�B	|�B	{�B	}�B	�B	��B	��B	��B	�B	�	B	�$B	�7B	�8B	�.B	�6B	�AB	�PB	�SB	�ZB	�`B	�fB	�mB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�%B	�(B	�)B	�(B	�1B	�8B	�8B	�;B	�CB	�MB	�;B	�7B	�/B	�)B	�*B	�1B	�7B	�7B	�7B	�6B	�8B	�6B	�:B	�>B	�=B	�JB	�LB	�[B	�aB	�oB	�lB	�oB	�lB	�mB	�wB	ǀB	ǀB	ǁB	ȆB	ɌB	ʐB	˙B	̝B	̞B	̞B	̝B	̝B	ΪB	ѽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�)B	�5B	�CB	�DB	�NB	�OB	�VB	�TB	�aB	�sB	�tB	�uB	�sB	�wB	�wB	�zB	�B	�~B	�B	�B	�{B	�zB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

B

B
B
B
B
B
&B
+B
*B
)B
0B
1B
/B
1B
6B
BB
DB
DB
HB
OB
OB
MB
PB
NB
NB
NB
OB
NB
OB
UB
SB
WB
[B
[B
]B
bB
YB
ZB
aB
lB
mG�O�B
tB
�B
#�B
)�B
7B
<8B
?GB
ElB
J�B
O�B
V�B
Z�B
^B
bB
d'B
jLB
ncB
s�B
w�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451262016080714512620160807145126  AO  ARCAADJP                                                                    20160202201806    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160202201806  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160202201806  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145126  IP                  G�O�G�O�G�O�                