CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  E   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:37Z AOML 3.0 creation; 2016-08-07T21:17:08Z UW 3.1 conversion     
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
resolution        =���   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ?�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  E   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       FX   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Kl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  P�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Q�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  V�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       X$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ]8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  bL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  h�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       i�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  o   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    o4   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    r4   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    u4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  x4   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    x`   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    xd   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    xh   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    xl   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  xp   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    x�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    x�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    x�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         x�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         x�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        x�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    x�Argo profile    3.1 1.2 19500101000000  20150226221237  20160807141708  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_002                   2C  D   APEX                            6487                            072314                          846 @���^ 1   @��s��@-�9XbN�c��t�j1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   B   B   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$�C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�3Ct�fCz  C  C��C���C�  C�� C��3C�s3C��3C�� C�  C�� C�  C�� C�  C�� C�  C�� C�  C�� C�  C�� C�  C���C�  C�� C�  C�� C�  Cŀ C�  Cʀ C��3C�s3C�  CԀ C�  Cـ C�  Cހ C�  C� C�  C� C�  C� C�  C� C�  C�� C�  C�� C�  D � D  D@ D� D� D  DFfD	� D
� D  D@ D� D��D  D@ D� D� D  D@ D� D� D  D@ D� D� D   D!@ D"� D#� D%  D&@ D'� D(� D*  D+@ D,� D-� D/  D0@ D1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<�fD>  D?@ D@� DA� DC  DD9�DE� DF� DH  DI@ DJ� DK��DM  DN@ DO� DP� DR  DS@ DT� DU� DW  DX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  DlFfDm�fDn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� Dz  Dz��D|y�D~��D���D���D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�34@љ�A��A(��AH��Ah��A�ffA�ffA�ffA�ffA�ffA�ffA�ffA�ffB33B
33B33B33B"33B*33B233B:33BB��BJ33BR33BZ33Bb33Bj33Br33Bz33B��B��B��B��B�L�B�� B��4B��gB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C �gC"��C$�gC&��C(��C*��C,��C.��C0��C2��C4s3C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr@ Cus3Cz��C��C�S3C�� C�FfC��fC�9�C���C�9�C��fC�FfC��fC�FfC��fC�FfC��fC�FfC��fC�FfC��fC�FfC��fC�FfC��3C�FfC��fC�FfC��fC�FfC��fC�FfC��fC�9�CϹ�C�FfC��fC�FfC��fC�FfC��fC�FfC��fC�FfC��fC�FfC��fC�FfC��fC�FfC��fC�FfC��fC�FfD �3D#3Dc3D�3D�3D#3Di�D	�3D
�3D#3Dc3D�3D��D#3Dc3D�3D�3D#3Dc3D�3D�3D#3Dc3D�3D�3D #3D!c3D"�3D#�3D%#3D&c3D'�3D(�3D*#3D+c3D,�3D-�3D/#3D0c3D1�3D2�3D4#3D5c3D6�3D7�3D9#3D:c3D;�3D<�D>#3D?c3D@�3DA�3DC#3DD\�DE�3DF�3DH#3DIc3DJ�3DK��DM#3DNc3DO�3DP�3DR#3DSc3DT�3DU�3DW#3DXc3DY�3DZ�3D\#3D]c3D^�3D_�3Da#3Dbc3Dc�3Dd�3Df#3Dgc3Dh�3Di�3Dk#3Dli�Dm��Dn�3Dp#3Dqc3Dr�3Ds�3Du#3Dvc3Dw�3Dx�3Dz#3D{ D|��D�D��gD�gD�Q�D���D��gD�gD�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�Q�D���D�� D�gD�NgD���D�њD��D�Q�D���D�њD��D�Q�D���D�њD��D�NgD���D�њD��D�Q�D���D�њD��D�Q�D���D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�E�A�E�A�E�A�C�A�C�A�E�A�C�A�C�A�;dA��A���Aڥ�Aڡ�AڬAڲ-A��A�
=A�/A�$�A� �A��A��HA�|�A�l�A�^5A�"�A�ƨAټjAٙ�A�v�A�v�A؇+A�"�A�x�A��#A���A�A�7LA�n�A���AuA��A��RA�A�ĜA�v�A���A��HA��uA��+A���A�dZA�l�A��mA���A���A�=qA�x�A���A���A�I�A��/A�5?A��A��hA�p�A�n�A�r�A���A�9XA�l�A��/A���A��uA��A�&�A��-A��!A��A�v�A��A���A���A��A�K�A���A��7A��A���Az��Aw�FAr^5Al5?Ah��Ad�A`�A\JAW�
AW|�AV^5AP��AJ�9AG��AEG�ACS�AA�
A@=qA>n�A;|�A9�
A9/A9oA8��A6�\A4�!A3ƨA333A2$�A1�A0{A.~�A*�9A)ƨA'�hA(jA"�DAA�-A�A|�A�RA�`A�FA�A|�A�A�yA(�A��A�Av�AdZA�AoA
�\A	�^A�`A��A$�A��A`BA;dA�AbA  A�FA�FAl�A
=A�+A��A��AQ�A{A�A Q�@��R@��/@��+@��@�Z@�=q@���@�u@�o@�+@���@���@�+@�9@�@�+@�~�@�I�@ޟ�@�n�@�X@ۅ@ى7@ם�@�5?@���@�ƨ@�O�@�Z@�  @Ͼw@θR@��@�`B@�S�@�n�@��@ț�@�"�@�v�@ă@Å@��y@�@�V@�Q�@��
@���@�o@�v�@��T@���@���@�v�@��@�&�@��u@�dZ@��@��@��@��@�@�hs@��/@���@�~�@���@��@�I�@���@�\)@��@�^5@�x�@��@�I�@��P@��!@��@�O�@�Z@�dZ@��\@���@��`@�K�@�~�@���@�X@�Z@�t�@�@�&�@� �@��@�5?@��@�&�@��9@��
@�"�@�=q@��^@�p�@���@���@�33@���@���@�ȴ@�J@�7L@�Q�@�|�@��+@�%@�ƨ@��@���@��@��D@K�@}O�@{�@y��@x��@w�@u�T@s�m@rn�@p�@n�R@l�j@kS�@j-@i��@g�@f$�@c�m@b�H@a�@_�@]�h@\(�@Z^5@X�`@W�P@T�j@R=q@PbN@N��@LI�@JM�@H�9@G�;@Fȴ@EO�@C�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111   A�E�A�E�A�E�A�C�A�C�A�E�A�C�A�C�A�;dA��A���Aڥ�Aڡ�AڬAڲ-A��A�
=A�/A�$�A� �A��A��HA�|�A�l�A�^5A�"�A�ƨAټjAٙ�A�v�A�v�A؇+A�"�A�x�A��#A���A�A�7LA�n�A���AuA��A��RA�A�ĜA�v�A���A��HA��uA��+A���A�dZA�l�A��mA���A���A�=qA�x�A���A���A�I�A��/A�5?A��A��hA�p�A�n�A�r�A���A�9XA�l�A��/A���A��uA��A�&�A��-A��!A��A�v�A��A���A���A��A�K�A���A��7A��A���Az��Aw�FAr^5Al5?Ah��Ad�A`�A\JAW�
AW|�AV^5AP��AJ�9AG��AEG�ACS�AA�
A@=qA>n�A;|�A9�
A9/A9oA8��A6�\A4�!A3ƨA333A2$�A1�A0{A.~�A*�9A)ƨA'�hA(jA"�DAA�-A�A|�A�RA�`A�FA�A|�A�A�yA(�A��A�Av�AdZA�AoA
�\A	�^A�`A��A$�A��A`BA;dA�AbA  A�FA�FAl�A
=A�+A��A��AQ�A{A�A Q�@��R@��/@��+@��@�Z@�=q@���@�u@�o@�+@���@���@�+@�9@�@�+@�~�@�I�@ޟ�@�n�@�X@ۅ@ى7@ם�@�5?@���@�ƨ@�O�@�Z@�  @Ͼw@θR@��@�`B@�S�@�n�@��@ț�@�"�@�v�@ă@Å@��y@�@�V@�Q�@��
@���@�o@�v�@��T@���@���@�v�@��@�&�@��u@�dZ@��@��@��@��@�@�hs@��/@���@�~�@���@��@�I�@���@�\)@��@�^5@�x�@��@�I�@��P@��!@��@�O�@�Z@�dZ@��\@���@��`@�K�@�~�@���@�X@�Z@�t�@�@�&�@� �@��@�5?@��@�&�@��9@��
G�O�@�=q@��^@�p�@���@���@�33@���@���@�ȴ@�J@�7L@�Q�@�|�@��+@�%@�ƨ@��@���@��@��D@K�@}O�@{�@y��@x��@w�@u�T@s�m@rn�@p�@n�R@l�j@kS�@j-@i��@g�@f$�@c�m@b�H@a�@_�@]�h@\(�@Z^5@X�`@W�P@T�j@R=q@PbN@N��@LI�@JM�@H�9@G�;@Fȴ@EO�@C�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBE�BE�BE�BE�BE�BE�BE�BF�BG�BL�BO�B_;Bk�Bp�Bs�B}�B�7B�{B��B��B��B�{B��B��B��B��B��B��B��B��B��B�BW
B6FB%�B+B]/B�XB�sB��B��B��B�B6FBF�BbNBk�B�B�7B�JB�\B��B��B��B�B�'B�B�'B�B��B�bBt�Be`BYBJ�B7LB+B�B{B�BuBPB	7B��B��B�'B��B�PBiyB]/BK�B6FB'�B�B
��B
�B
��B
cTB	��B	�B	�\B	l�B	P�B	D�B	7LB	)�B	�B	1B	B��B�sB��BɺBÖBB��B�wB�dB�^B�dB�jB�dB�^B�wBÖBŢBŢBÖBÖBȴB��B��B��B	hB	L�B	$�B	B	{B	!�B	+B	5?B	T�B	\)B	y�B	��B	�+B	~�B	�VB	��B	�B	��B	�B	�wB	��B	��B	�)B	�NB	�B	�B	�B	��B	��B	��B	��B
B
	7B
\B
bB
VB
VB
\B

=B
DB
JB
	7B
+B
B
B	��B	��B	�B	�B	�yB	�TB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�mB	�`B	�sB	�B	�sB	�`B	�`B	�`B	�`B	�`B	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
+B
+B
+B
+B
+B
1B
	7B

=B

=B
DB
JB
JB
PB
VB
VB
\B
VB
bB
hB
hB
hB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
$�B
%�B
%�B
'�B
(�B
)�B
+B
-B
-B
.B
/B
0!B
1'B
2-B
33B
49B
5?B
5?B
7LB
8RB
:^B
;dB
=qB
>wB
>wB
?}B
@�B
@�B
B�B
C�B
D�B
E�B
F�B
F�B
G�B
H�B
J�B
J�B
K�B
M�B
N�B
O�B
Q�B
R�B
S�B
S�B
T�B
VB
W
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111   BEwBEyBEwBEwBEwBEwBEwBFBG�BL�BO�B_BkZBp{Bs�B}�B�B�QB�bB�iB�`B�QB�B��B��B�vB�XB�_B�_B�cB�VB��BV�B6B%�B*�B]B�(B�EB��B��B��ByB6BFwBb BkSB��B�
B�B�,B��B��B��B��B��B��B��B��B��B�1Bt�Be/BX�BJ�B7B*�BuBHBVB@BB	B�BѺB��B��B�BiEB\�BK�B6B'�BLB
��B
��B
��B
c$B	��B	��B	�0B	l_B	P�B	DrB	7#B	)�B	^B	B	�B��B�HB��BɑB�mB�fB�YB�NB�:B�4B�9B�@B�;B�5B�JB�kB�uB�vB�lB�mBȊBйB��B��B	8B	L�B	$�B	�B	GB	!�B	*�B	5B	T�B	[�B	y�B	�UB	��B	~�B	�B	��B	��B	��B	��B	�>B	ˋB	ѴB	��B	�B	�RB	�PB	�qB	�B	�B	��B	��B
�B
�B
!B
%B
B
B
 B

B
	B
B
�B
�B
�B
�B	��B	��B	�OB	�HB	�;B	�B	�#B	�OB	�XB	�_B	�`B	�PB	�GB	�GB	�HB	�1B	�#B	�7B	�HB	�7B	�"B	�"B	�$B	�"B	� B	�.B	�;B	�;B	�<B	�GB	�NB	�NB	�RB	�QB	�LB	�JB	�GB	�GB	�MB	�RB	�`B	�dB	�lB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B
B
B
	B
B
B
B
B
B
B
(B
'B
%B
/B
4B
@B
IB
LB
QB
ZB
_B
`B
^B
eG�O�B
qB
wB
uB
vB
wB
 �B
!�B
#�B
$�B
%�B
%�B
'�B
(�B
)�B
*�B
,�B
,�B
-�B
.�B
/�B
0�B
1�B
2�B
3�B
4�B
4�B
7B
8B
:B
;!B
=0B
>4B
>4B
?;B
@=B
@@B
BMB
CUB
D[B
E_B
FfB
FdB
GnB
HqB
J~B
J~B
K�B
M�B
N�B
O�B
Q�B
R�B
S�B
S�B
T�B
U�B
V�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.55 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417082016080714170820160807141708  AO  ARCAADJP                                                                    20150226221237    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221237  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221237  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141708  IP                  G�O�G�O�G�O�                