CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:49Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125949  20190405100756  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��z��1   @��{��B@/��
=p��deG�z�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33BǙ�B���B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B���C  C  C  C  C
  C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyS3D�fD�FfD��fD���D�3D�<�D�|�D��3D���D�L�D��fD���D� D�FfDډ�D�3D� D�VfD�p D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�\B�\B�u�B̨�B��)B�\B��)B��)B��)B��B��)B��)B��)B��)B�\B�\C T{CnCnCnCnC
nCnCTzCnCnCnCnCnCnCnCnC nC"nC$nC&��C(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>TzC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCT��CVnCXnCZTzC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
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
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�RDyn�D�$)D�T)D��)D��D��D�J�D���D���D�]D�Z�D��)D�ڐD��D�T)Dڗ]D���D��D�d)D�}�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A���A���A���A���A��A��A��yA��`A��HA��TA��#A��/A��#A��A��A��A��A��A���A��
A��
A���A���A�ȴA���AڸRAڏ\A�p�A�E�A�A��
A���A���A�ȴA�ĜAټjAٺ^AټjAټjAٺ^Aٺ^A٩�AٍPA�O�A؇+A�VA�ĜA�dZA�-A�~�A���A�r�A�`BA�x�A�
=A�?}A�r�A��HA��FA�r�A��uA��jA�?}A��A��uA��
A�ȴA�|�A�1A��hA��A�1A�5?A��A��RA���A��A�t�A��A��A��A�r�A�
=A��PA�bA�K�A��A�p�A�  A�t�A�
=A��A�A�oA��TA��;A�r�A�v�A~~�Ay"�AvbNAq?}Aj=qAfAc��A]�FAR��APjALȴAJȴAIG�AD��AB�9A>n�A<�A:�HA:-A8n�A5�PA5��A3`BA1x�A1\)A.�A*ȴA)O�A(�`A&�A%C�A#VA"^5A!�FA bA��A+A�+A�AM�A�AXA��A��AhsA�AjAhsA��A�A�-A��A�AVA��A`BA��A	��AA�AhsA  A��A"�Az�Ap�A �/A {@�l�@���@�hs@��/@�j@���@���@���@�5?@��@��#@���@�`B@�b@�"�@���@�9X@�l�@�@��@�bN@�K�@�V@�O�@�A�@�|�@�o@�F@�@�M�@�ƨ@�X@��y@�!@�M�@�@�7@�/@��@��H@�7L@��@�1@�;d@�+@���@�!@�!@��H@�33@�J@��H@�S�@�|�@��@�F@���@�9X@�z�@��@��@���@�R@�v�@���@�@�O�@�Ĝ@�z�@�ƨ@�S�@�@��y@�R@�v�@���@�@��@�@���@�t�@�;d@�ff@�x�@���@��@�{@��/@ە�@��@ج@؋D@�9X@�b@��@�~�@���@���@��/@��/@ԃ@ӥ�@�K�@���@ҟ�@�=q@ѡ�@�?}@�V@�hs@��@Χ�@�=q@�-@�M�@�M�@�{@Ͳ-@́@��`@˥�@�S�@�K�@�dZ@�;d@��@ɩ�@Ɂ@�X@�?}@��@��`@ȣ�@ȓu@Ȭ@ȃ@ǍP@ǥ�@�33@�33@�S�@�K�@�ff@�=q@���@��@�&�@�A�@�"�@���@�^5@��@��@���@��@�  @��@��@��R@�ff@�5?@�X@��9@�9X@��F@�dZ@��H@�-@�@���@�p�@�%@��j@�9X@���@���@��@��y@���@�5?@�V@�n�@�v�@���@�`B@���@���@��@�j@�  @��@�l�@�K�@�C�@�o@���@��+@�M�@�{@���@��7@���@���@�X@�&�@���@���@��u@�(�@�  @�ƨ@��F@���@�\)@�^5@��T@��^@���@�`B@���@�9X@�  @��m@��
@��@�K�@�o@��+@��T@�&�@�Ĝ@�Z@�S�@��\@��@���@�/@��@��@�C�@��@��@�V@�@��@���@�?}@���@���@�j@�1'@� �@�ƨ@��P@�\)@��y@�~�@�E�@�@��@���@��-@��@�7L@��9@��/@���@���@��D@�z�@�r�@�Z@�I�@�9X@�1'@�  @�C�@���@��\@�ff@�M�@�E�@�=q@�-@�-@�$�@��@�{@���@���@�`B@�%@��j@�1'@�b@��;@��P@��@�E�@�{@��#@���@�x�@�`B@���@��u@�bN@�9X@��@�  @��@��w@�S�@��@��R@���@�~�@�J@���@�G�@��@�%@��!@���@|�@r��@ihs@_|�@W�@O
=@F@;33@41@/K�@)�^@#t�@��@�y@�@��@33@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A���A���A���A���A��A��A��yA��`A��HA��TA��#A��/A��#A��A��A��A��A��A���A��
A��
A���A���A�ȴA���AڸRAڏ\A�p�A�E�A�A��
A���A���A�ȴA�ĜAټjAٺ^AټjAټjAٺ^Aٺ^A٩�AٍPA�O�A؇+A�VA�ĜA�dZA�-A�~�A���A�r�A�`BA�x�A�
=A�?}A�r�A��HA��FA�r�A��uA��jA�?}A��A��uA��
A�ȴA�|�A�1A��hA��A�1A�5?A��A��RA���A��A�t�A��A��A��A�r�A�
=A��PA�bA�K�A��A�p�A�  A�t�A�
=A��A�A�oA��TA��;A�r�A�v�A~~�Ay"�AvbNAq?}Aj=qAfAc��A]�FAR��APjALȴAJȴAIG�AD��AB�9A>n�A<�A:�HA:-A8n�A5�PA5��A3`BA1x�A1\)A.�A*ȴA)O�A(�`A&�A%C�A#VA"^5A!�FA bA��A+A�+A�AM�A�AXA��A��AhsA�AjAhsA��A�A�-A��A�AVA��A`BA��A	��AA�AhsA  A��A"�Az�Ap�A �/A {@�l�@���@�hs@��/@�j@���@���@���@�5?@��@��#@���@�`B@�b@�"�@���@�9X@�l�@�@��@�bN@�K�@�V@�O�@�A�@�|�@�o@�F@�@�M�@�ƨ@�X@��y@�!@�M�@�@�7@�/@��@��H@�7L@��@�1@�;d@�+@���@�!@�!@��H@�33@�J@��H@�S�@�|�@��@�F@���@�9X@�z�@��@��@���@�R@�v�@���@�@�O�@�Ĝ@�z�@�ƨ@�S�@�@��y@�R@�v�@���@�@��@�@���@�t�@�;d@�ff@�x�@���@��@�{@��/@ە�@��@ج@؋D@�9X@�b@��@�~�@���@���@��/@��/@ԃ@ӥ�@�K�@���@ҟ�@�=q@ѡ�@�?}@�V@�hs@��@Χ�@�=q@�-@�M�@�M�@�{@Ͳ-@́@��`@˥�@�S�@�K�@�dZ@�;d@��@ɩ�@Ɂ@�X@�?}@��@��`@ȣ�@ȓu@Ȭ@ȃ@ǍP@ǥ�@�33@�33@�S�@�K�@�ff@�=q@���@��@�&�@�A�@�"�@���@�^5@��@��@���@��@�  @��@��@��R@�ff@�5?@�X@��9@�9X@��F@�dZ@��H@�-@�@���@�p�@�%@��j@�9X@���@���@��@��y@���@�5?@�V@�n�@�v�@���@�`B@���@���@��@�j@�  @��@�l�@�K�@�C�@�o@���@��+@�M�@�{@���@��7@���@���@�X@�&�@���@���@��u@�(�@�  @�ƨ@��F@���@�\)@�^5@��T@��^@���@�`B@���@�9X@�  @��m@��
@��@�K�@�o@��+@��T@�&�@�Ĝ@�Z@�S�@��\@��@���@�/@��@��@�C�@��@��@�V@�@��@���@�?}@���@���@�j@�1'@� �@�ƨ@��P@�\)@��y@�~�@�E�@�@��@���@��-@��@�7L@��9@��/@���@���@��D@�z�@�r�@�Z@�I�@�9X@�1'@�  @�C�@���@��\@�ff@�M�@�E�@�=q@�-@�-@�$�@��@�{@���@���@�`B@�%@��j@�1'@�b@��;@��P@��@�E�@�{@��#@���@�x�@�`B@���@��u@�bN@�9X@��@�  @��@��w@�S�@��@��R@���@�~�@�J@���@�G�@��@�%@��!@���@|�@r��@ihs@_|�@W�@O
=@F@;33@41@/K�@)�^@#t�@��@�y@�@��@33@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	\)B	]/B	]/B	\)B	\)B	\)B	\)B	\)B	\)B	\)B	\)B	\)B	\)B	[#B	\)B	\)B	\)B	[#B	[#B	[#B	[#B	[#B	[#B	[#B	[#B	ZB	[#B	ZB	YB	ZB	[#B	`BB	e`B	ffB	gmB	gmB	hsB	gmB	hsB	hsB	hsB	iyB	jB	jB	jB	jB	m�B	q�B	q�B	x�B	��B	�)B	�`B
B
{B
#�B
H�B
O�B
s�B
}�B
�DB
�uB
�{B
�B
��B
�B
��B�BXB�B��B��B�oB�7B~�Bt�B_;BL�BE�B7LB(�B{B
��B
�B
ȴB
�FB
��B
�B
aHB
ZB
Q�B
H�B
>wB
&�B
VB	�B	�5B	��B	ɺB	�jB	��B	�1B	w�B	`BB	@�B	33B	(�B	uB��B�B�B�B�B�B�fB�B�mB�fB�fB�mB�sB�B�fB�HB�TB�5B��BƨBBƨBǮB��B��B��B��B��B��B��B��B��BȴBƨBȴB�NB�B��B��B��B��B��B�B�B�`B�B�B�ZB�B��B��B�^B�9B�'B�LB�XB�XB�XB��BŢB��B�B�)B�/B�5B�5B�HB�TB�yB�B�B��B��B��B��B	B	B	%B	
=B	uB	�B	�B	{B	�B	�B	�B	)�B	2-B	A�B	M�B	YB	ffB	l�B	k�B	iyB	iyB	gmB	ffB	gmB	ffB	bNB	]/B	ZB	ZB	ZB	\)B	]/B	_;B	bNB	ffB	{�B	�%B	�DB	�\B	�hB	��B	��B	��B	��B	��B	�B	�-B	�-B	�3B	�?B	�9B	�3B	�?B	�LB	�XB	�^B	�dB	�dB	�jB	�}B	B	B	B	B	ÖB	ÖB	B	B	ȴB	��B	��B	ɺB	ɺB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�#B	�/B	�/B	�/B	�)B	�)B	�#B	�#B	�#B	�#B	�BB	�NB	�`B	�fB	�fB	�ZB	�fB	�fB	�B	�B	�B	�B	�B	�B	�sB	�B	�B	�mB	�fB	�fB	�fB	�fB	�`B	�`B	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�mB	�mB	�mB	�mB	�yB	�B	�yB	�yB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
  B
  B
B
B
B
B
B
  B
B
B
B
B
B
+B
	7B
1B
1B
1B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B

=B
DB
DB
DB
DB
DB
DB
DB
\B
bB
hB
hB
hB
hB
oB
oB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
{B
uB
uB
{B
{B
uB
uB
uB
uB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
 �B
%�B
49B
9XB
:^B
>wB
C�B
J�B
O�B
YB
^5B
aHB
ffB
k�B
o�B
u�B
x�B
|�B
�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	\	B	]B	]B	\B	\B	\
B	\B	\B	\B	\B	\B	\B	\B	[B	\B	\B	\B	[ B	[B	[B	[B	[B	[B	[B	[B	Y�B	[B	Y�B	X�B	Y�B	[B	`B	e>B	fDB	gMB	gKB	hPB	gLB	hRB	hSB	hRB	iVB	j_B	j`B	j^B	j_B	moB	q�B	q�B	x�B	��B	�B	�>B
 �B
YB
#�B
H�B
O�B
s�B
}�B
� B
�RB
�VB
��B
ϻB
�zB
��B~BW�B��B�yB�gB�GB�B~�Bt�B_BL�BE{B7$B(�BUB
��B
��B
ȌB
�B
�{B
��B
aB
Y�B
Q�B
H�B
>LB
&�B
/B	�fB	�
B	иB	ɍB	�=B	��B	�B	w�B	`B	@UB	3B	(�B	FB��B�{B�gB�aB�UB�NB�5B�OB�>B�4B�6B�=B�BB�tB�5B�B�"B�BдB�wB�]B�vB�|BдBҽBҿB��BвBϭBΥB̚BʐBȃB�vB�B�B�KB��B��B��B��B��B�B�[B�,B�vB�eB�$B��BʌB�QB�*B�B��B�B�#B�#B�$B�OB�mBʎB��B��B��B� B�B�B� B�AB�hB�tB�B��B��B��B	 �B	�B	�B	
B	@B	HB	IB	GB	VB	]B	�B	)�B	1�B	AVB	M�B	X�B	f2B	lVB	kPB	iDB	iCB	g:B	f/B	g9B	f0B	bB	\�B	Y�B	Y�B	Y�B	[�B	\�B	_B	bB	f0B	{�B	��B	�B	�$B	�1B	�^B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	��B	�B	�B	�#B	�)B	�.B	�.B	�5B	�IB	�YB	�XB	�XB	�WB	�bB	�_B	�ZB	�XB	�|B	ʍB	˒B	ɁB	ɄB	�pB	�sB	ɅB	ʉB	͛B	ϦB	ѷB	ЬB	ϩB	ЬB	ЬB	ϫB	бB	ЯB	ЭB	ЮB	ѷB	ѶB	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�)B	�-B	�/B	�!B	�1B	�0B	�GB	�LB	�OB	�HB	�HB	�IB	�<B	�HB	�GB	�3B	�0B	�/B	�/B	�0B	�*B	�*B	�B	�B	�$B	�+B	�/B	�5B	�9B	�6B	�5B	�7B	�5B	�AB	�HB	�BB	�CB	�<B	�CB	�BB	�EB	�IB	�SB	�YB	�iB	�_B	�YB	�lB	�sB	�B	�B	�~B	�B	�~B	�wB	�wB	�xB	�~B	�B	�}B	�B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B	��B	��B
 �B
 �B
 �B
 �B
 �B	��B
 �B
�B
�B
�B
�B
�B
	 B
�B
�B
�B
�B
	 B

B

B

B


B

B

B

B

B

B

B

B

B
B
B
B
B
B
B
%B
+B
1B
1B
0B
2B
7B
4B
7B
=B
<B
CB
IB
IB
GB
OB
PB
HB
NB
NB
NB
PB
HB
LB
MB
PB
NB
NB
MB
PB
OB
IB
JB
IB
GB
CB
DB
=B
=B
BB
EB
<B
<B
=B
=B
=B
<B
>B
DB
CB
EB
IB
IB
HB
LG�O�B
bB
 �B
%�B
3�B
9B
:&B
>=B
C^B
J�B
O�B
X�B
]�B
aB
f-B
kLB
oeB
u�B
x�B
|�B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007562019040510075620190405100756  AO  ARCAADJP                                                                    20181121125949    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125949  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125949  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100756  IP                  G�O�G�O�G�O�                