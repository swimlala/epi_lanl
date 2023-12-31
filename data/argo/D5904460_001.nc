CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  B   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:36Z AOML 3.0 creation; 2016-08-07T21:17:08Z UW 3.1 conversion     
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
resolution        =���   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ?�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       F8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       K@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  PH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Q�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  V�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       W�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       \�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  a�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 D  h4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ix   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  n�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    n�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    q�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    t�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  w�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    w�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    w�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    w�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    w�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  w�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    x,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    x<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    x@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         xP   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         xT   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        xX   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    x\Argo profile    3.1 1.2 19500101000000  20150226221236  20160807141708  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_001                   2C  D   APEX                            6487                            072314                          846 @���L��1   @��`� @. �n���c�p��
=1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq��Cu  Cz  C  C�  C���C��C�s3C��3C�� C�  C�� C�  C�� C�  C�� C�  C�� C�  C�� C�  C�� C��C���C�  C�� C�  C�� C�  C�� C�  Cŀ C�  Cʀ C�  Cπ C�  CԀ C�  Cـ C�  Cހ C�  C� C�  C� C�  C� C�  C� C�  C�� C�  C�� C�  D � D  D@ D� D� D  D@ D	� D
� D  DFfD� D� D  D@ D� D� D  D@ D� D� D  D@ D� D� D   D!@ D"� D#� D%  D&@ D'� D(� D*  D+@ D,� D-� D/fD0FfD1� D2� D4  D5@ D6� D7� D9  D:@ D;� D<� D>  D?@ D@� DA� DC  DD@ DE� DF� DH  DI@ DJ� DK� DM  DN@ DO� DP� DR  DS@ DT� DU� DWfDX@ DY� DZ� D\  D]@ D^� D_� Da  Db@ Dc� Dd� Df  Dg@ Dh� Di� Dk  Dl@ Dm� Dn� Dp  Dq@ Dr� Ds� Du  Dv@ Dw� Dx� DzfDz�3D|� D  D�� D�  D�C3D��fD���D���D�<�D�|�D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�ɚD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�S31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@��
A	�A)�AI�Ai�A���A���A���A���A���A���A���A���Bz�B
z�Bz�Bz�B"z�B*z�B2z�B:z�BBz�BJz�BRz�BZz�Bbz�Bjz�Brz�Bzz�B�p�B�p�B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�p�B�p�B�=qB�
>B�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qB�=qC ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,�RC.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Crk�Cu��Cz��C��C�O\C��)C�h�C�C�B�C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�\)C��)C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\C��\C�O\D �D'�Dg�D��D�D'�Dg�D	��D
�D'�DnD��D�D'�Dg�D��D�D'�Dg�D��D�D'�Dg�D��D�D '�D!g�D"��D#�D%'�D&g�D'��D(�D*'�D+g�D,��D-�D/.D0nD1��D2�D4'�D5g�D6��D7�D9'�D:g�D;��D<�D>'�D?g�D@��DA�DC'�DDg�DE��DF�DH'�DIg�DJ��DK�DM'�DNg�DO��DP�DR'�DSg�DT��DU�DW.DXg�DY��DZ�D\'�D]g�D^��D_�Da'�Dbg�Dc��Dd�Df'�Dgg�Dh��Di�Dk'�Dlg�Dm��Dn�Dp'�Dqg�Dr��Ds�Du'�Dvg�Dw��Dx�Dz.D{�D|��D'�D���D��D�W
D��=D�ФD��D�P�D���D���D��D�S�D���D�ФD��D�S�D���D���D��D�S�D���D���D��D�S�D���D��qD��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D��D�S�D���D���D�
D�g
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A۶FA۴9A۩�Aۣ�Aۥ�Aە�A�t�A�VA� �A��mA��A��A���AھwA�t�A�x�A�p�A�A�A�"�A�{A���Aٰ!A١�Aٟ�Aٟ�Aٟ�Aٟ�A�p�A�%A���A��TA��/A��AÅA�hsA�1A���A�r�A��uA���A�Q�A��uA�A�bNA��\A��A�I�A���A��uA�bNA�ĜA��+A��hA�A��jA�
=A�K�A��`A���A���A�C�A�oA���A��TA�v�A�JA���A�C�A�/A�bA�dZA�x�A�l�A�t�A���A��!A�x�A���A�1A��\A�33A�dZA�ȴAu�;Al1'Ah��Ad  Ac�Ab��A^��AW��AP~�AL��AH�+AEO�AD�jACG�A@�A=��A:�`A9��A9�A8�uA7�A5%A4  A3�A2A�A1dZA0I�A/�A/`BA.ffA-A,�yA,��A,ȴA,��A+��A+�A(~�A&�/A"��A�;AdZA��AG�A�#A��A��A(�A�TA�A+A&�AC�A��A�AVAA��AJA��A
v�A��A�`AĜAZA��A+A"�A/A
=A�!A(�A7LA33AffAoA�-A �@��
@���@�/@�ȴ@�G�@�C�@��/@�@�{@���@�bN@��y@���@�w@�;d@�!@�J@�r�@���@�j@�F@�^5@�r�@�ff@�7L@܋D@�;d@ٙ�@ؼj@�@�r�@�  @�33@�M�@�7L@�9X@�ƨ@�J@˾w@��#@�bN@�l�@��@�~�@��@��@ģ�@���@�^5@��@��@�o@��+@��7@���@���@�"�@�J@�/@�z�@���@�l�@��@�x�@���@���@�@�V@���@��@�1@��@���@�ƨ@��@�5?@�/@�A�@��@�+@�n�@�@���@��@��D@�b@���@��7@���@��@�dZ@���@��@��@���@�ƨ@�
=@���@�7L@��j@�Q�@��P@��H@�n�@�x�@�j@��P@��R@�J@���@���@��j@��@�{@�j@�@�=q@���@��P@�n�@�hs@�A�@~@|1@{o@y�^@x�9@v�@u�@s�
@rJ@pbN@o��@n@kt�@i�^@hQ�@fv�@d�D@co@a&�@_|�@]`B@[ƨ@Y��@X1'@V@Tj@R��@P�`@Ol�@M?}@L(�@K33@I��@Hr�@G;d@FE�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111  A���A���A���A۶FA۴9A۩�Aۣ�Aۥ�Aە�A�t�A�VA� �A��mA��A��A���AھwA�t�A�x�A�p�A�A�A�"�A�{A���Aٰ!A١�Aٟ�Aٟ�Aٟ�Aٟ�A�p�A�%A���A��TA��/A��AÅA�hsA�1A���A�r�A��uA���A�Q�A��uA�A�bNA��\A��A�I�A���A��uA�bNA�ĜA��+A��hA�A��jA�
=A�K�A��`A���A���A�C�A�oA���A��TA�v�A�JA���A�C�A�/A�bA�dZA�x�A�l�A�t�A���A��!A�x�A���A�1A��\A�33A�dZA�ȴAu�;Al1'Ah��Ad  Ac�Ab��A^��AW��AP~�AL��AH�+AEO�AD�jACG�A@�A=��A:�`A9��A9�A8�uA7�A5%A4  A3�A2A�A1dZA0I�A/�A/`BA.ffA-A,�yA,��A,ȴA,��A+��A+�A(~�A&�/A"��A�;AdZA��AG�A�#A��A��A(�A�TA�A+A&�AC�A��A�AVAA��AJA��A
v�A��A�`AĜAZA��A+A"�A/A
=A�!A(�A7LA33AffAoA�-A �@��
@���@�/@�ȴ@�G�@�C�@��/@�@�{@���@�bN@��y@���@�w@�;d@�!@�J@�r�@���@�j@�F@�^5@�r�@�ff@�7L@܋D@�;d@ٙ�@ؼj@�@�r�@�  @�33@�M�@�7L@�9X@�ƨ@�J@˾w@��#@�bN@�l�@��@�~�@��@��@ģ�@���@�^5@��@��@�o@��+@��7@���@���@�"�@�J@�/@�z�@���@�l�@��@�x�@���@���@�@�V@���@��@�1@��@���@�ƨ@��@�5?@�/@�A�@��@�+@�n�@�@���@��@��D@�b@���@��7@���@��@�dZ@���@��@��@���@�ƨ@�
=@���@�7L@��j@�Q�@��P@��HG�O�@�x�@�j@��P@��R@�J@���@���@��j@��@�{@�j@�@�=q@���@��P@�n�@�hs@�A�@~@|1@{o@y�^@x�9@v�@u�@s�
@rJ@pbN@o��@n@kt�@i�^@hQ�@fv�@d�D@co@a&�@_|�@]`B@[ƨ@Y��@X1'@V@Tj@R��@P�`@Ol�@M?}@L(�@K33@I��@Hr�@G;d@FE�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBP�BO�BM�BL�BL�BL�BK�BK�BJ�BI�BH�BI�BM�BS�B_;Br�B� B�+B�JB�bB��B��B��B��B��B�{B�uB�uB�uB�uB�\B�1B6FB6FB<jB~�B��B�jB�5B�B>wBaHBn�B�=B�{B��B��B��B��B��B��B��B��B�+B|�Bx�Bq�Bu�Br�Bl�BiyBhsB\)BP�BH�B?}B7LB.B)�B#�B�BJBVBB�sB��B��B��B�BW
B49BB
�/B
�+B
>wB	�;B	~�B	Q�B	O�B	?}B	:^B	1'B	�B	B�B�HB��B�wB�^B�RB�dB�RB�FB�FB�FB�?B�3BÖBǮBǮBȴBȴBɺB��B��B�)B�B�B�B��B��B	�B	(�B	-B	>wB	'�B	{B	�B	�B	-B	YB	hsB	��B	��B	�B	�JB	��B	��B	��B	�B	�XB	�qB	��B	�B	�#B	�BB	�#B	�B	�HB	�sB	�B	�B	�B	��B	��B
  B
B
B
	7B
bB
PB
	7B
+B
+B
B
%B
B
  B	��B	��B	�B	�B	�B	�B	�yB	�mB	�TB	�NB	�fB	�B	�B	�B	�B	�B	�B	�yB	�mB	�fB	�yB	�B	�mB	�ZB	�ZB	�BB	�;B	�NB	�ZB	�`B	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
+B
1B
	7B

=B

=B

=B
DB
DB
DB
JB
JB
JB
PB
VB
\B
\B
bB
bB
hB
hB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
#�B
$�B
&�B
(�B
)�B
+B
-B
-B
/B
0!B
1'B
2-B
2-B
33B
49B
5?B
7LB
8RB
9XB
:^B
;dB
<jB
<jB
>wB
?}B
@�B
@�B
B�B
B�B
D�B
F�B
G�B
H�B
I�B
J�B
K�B
L�B
M�B
M�B
O�B
P�B
Q�B
Q�B
R�B
S�B
V1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111  BP�BO�BM�BL�BL�BL�BK�BK�BJ�BI�BH�BI�BM�BS�B_Br�B�B��B�B�4B�SB�bB�bB�cB�TB�KB�BB�EB�GB�BB�+B� B6B6B<8B~�B��B�8B�BkB>ABaBnaB�B�DB��B��B��B��B��B��B��B�IB��B|�Bx�BqsBu�Br|BlSBiABh;B[�BP�BH|B?EB7B-�B)�B#�BYBBB �B�6BϤB��B�GB��BV�B3�B �B
��B
��B
>@B	�B	~�B	Q�B	O�B	?JB	:-B	0�B	{B	�B�_B�B͢B�HB�/B�B�4B�B�B�B�B�B� B�dB�vB�zBȀBȁBɅB̚BгB��B�[B�bB�sB��B��B	hB	(�B	,�B	><B	'�B	BB	kB	gB	,�B	X�B	h8B	�MB	�eB	��B	�B	�ZB	�LB	��B	��B	�B	�/B	ԺB	��B	��B	��B	��B	��B	�B	�0B	�9B	�ZB	�cB	�|B	��B	��B
 �B
�B
�B
B
B
�B
�B
�B
�B
�B
�B	��B	��B	�}B	�MB	�9B	�?B	�8B	�4B	�%B	�B	�	B	�!B	�LB	�LB	�SB	�EB	�>B	�7B	�3B	�'B	�"B	�2B	�8B	�&B	�B	�B	��B	��B	�B	�B	�B	�%B	�3B	�6B	�>B	�>B	�<B	�6B	�7B	�>B	�=B	�>B	�AB	�HB	�KB	�WB	�dB	�sB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
�B
�B
	�B
	�B
	�B

�B

�B

�B
�B
B
B
B
B
B
B
B
B
B
B
#B
#B
)B
1B
8B
<B
>B
EB
CG�O�B
HB
VB
\B
\B
^B
hB
 yB
!~B
#�B
$�B
&�B
(�B
)�B
*�B
,�B
,�B
.�B
/�B
0�B
1�B
1�B
2�B
3�B
4�B
6�B
8B
9B
:B
;B
<B
<B
>'B
?1B
@5B
@8B
BEB
BDB
DOB
F[B
GaB
HiB
ImB
JuB
K|B
L�B
M�B
M�B
O�B
P�B
Q�B
Q�B
R�B
S�B
U�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.62 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417082016080714170820160807141708  AO  ARCAADJP                                                                    20150226221236    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221236  QCP$                G�O�G�O�G�O�8FB5E           AO  ARGQQCPL                                                                    20150226221236  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141708  IP                  G�O�G�O�G�O�                