CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-12-17T10:00:45Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
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
resolution        =���   axis      Z        x  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    E�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  I   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    U�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  X�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  e   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    q�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                       SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  Ř   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �D        �DArgo profile    3.1 1.2 19500101000000  20191217100045  20200901151527  5905305 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               RA   AO  6986                            2C  D   NAVIS_A                         0833                            170425                          863 @�����1   @��"".�@0aG�z��e� ě�1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      RA   A   A   @���@�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C��3D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.�fD/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8�fD9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?�fD@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DH��DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg�fDhfDh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{y�D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�C3D��3D��3D�  D�<�D�|�D���D���D�@ D�� D�� D�3D�C3D�� D�� D�  D�<�D�� D��3D�  D�@ D�� D���D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D���D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D���D�  D�@ D�|�D�� D�  D�@ D�� D�� D���D�<�D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�3D�C3D��3D��3D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�|�D�� D�3D�C3D��3D�� D�  D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D��3D�� D���D�<�D�|�D���D���D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�|�D���D�  D�@ D�|�D���D�  D�C3D��3D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D���D�@ D D�� D�  D�@ D�|�D�� D�  D�@ D�|�D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D���D�@ Dǃ3Dǣ3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
>@�p�@�p�A�RA@Q�A^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B��
B�
=B�
=B��
B��
B��
B��
B��
B��
B�
=B��
B���B��
B���B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C\C^C`Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C��C��C��C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D
GD
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��D�GD��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D�GD��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"�{D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.�GD.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8�GD8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?�GD?��D@t{D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH�{DIz�DI��DJz�DJ��DKz�DK�{DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DZGDZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dg�GDhGDhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dxz�Dx��Dyz�Dy��Dzz�Dz��D{t{D{��D|z�D|��D}z�D}��D~z�D~��Dz�D��D�=qD�z>D��qD��qD�=qD�z>D��qD��qD�=qD�}qD��qD��qD�@�D���D���D��qD�:>D�z>D��>D��>D�=qD�}qD��qD� �D�@�D�}qD��qD��qD�:>D�}qD���D��qD�=qD�}qD��>D��qD�:>D�z>D��>D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�@�D���D���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��>D�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��>D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z>D��>D��>D�=qD�}qD��qD��qD�:>D�z>D��qD��qD�=qD�}qD��>D��qD�=qD�z>D��qD��qD�=qD�}qD��qD��>D�:>D�}qD���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD��>D�=qD�}qD��qD��>D�:>D�}qD��qD��qD�=qD�}qD��qD��qD�=qD�z>D��qD��qD�=qD�z>D��qD� �D�@�D���D���D� �D�=qD�z>D��qD��qD�=qD�}qD��qD��qD�=qD�}qD��qD��qD�:>D�}qD��qD��qD�=qD�z>D��qD� �D�@�D���D��qD��qD�:>D�z>D��>D��qD�=qD�}qD��qD��qD�=qD���D��qD��>D�:>D�z>D��>D��>D�=qD�}qD��qD��qD�@�D�}qD��qD��qD�=qD���D��qD��qD�=qD�}qD��qD� �D�=qD�}qD��qD��>D�:>D�}qD��qD��qD�=qD�z>D��>D��qD�=qD�z>D��>D��qD�@�D���D��qD��qD�=qD�}qD��qD� �D�@�D�}qD��qD��qD�:>D�}qD��qD��qD�=qD�}qD��qD��>D�=qD�}qD½qD��qD�=qD�z>DýqD��qD�=qD�z>DĽqD��qD�=qD�}qDŽqD��qD�=qD�}qDƽqD��>D�=qDǀ�DǠ�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�"�A�&�A�1'A�33A�1'A�1'A�/A�/A�-A�-A�/A�33A�5?A�5?A�=qA�A�A�C�A�E�A�G�A�I�A�O�A�O�A�K�A�?}A�5?A�+A�"�A�{A�{A�VA���A��TA���A�A�A��Aۏ\Aڧ�A��A�  A��A�JA�bA��HAǕ�A��A��A�oA�`BA���A��A��A��A�Q�A�^5A�I�A��7A�&�A�JA��/A���A�C�A�l�A���A�M�A��A�VA�bNA���A��wA���A��A���A�ȴA�p�A�?}A���A��uA�oA�hsA���A��9A�z�A�M�A���A�{A�7LA��A���A��A�
=A�PA{S�AxȴAwG�At-ApbNAl-Ag�mAf�DAe&�A_t�A]O�AY�
AW&�AU�PAT1APv�ANZAL5?AJ�AH{ADr�AC��AA+A?�;A?VA<r�A7"�A5VA3hsA1�PA0�A/?}A.ffA,�uA,r�A,ZA,(�A,$�A, �A,1A+�A*r�A(1'A'�A%�A"�\A!�FA �9A�TAoA�A/A�A(�A��A�7At�AXAoAA(�A��A|�A`BA&�AVA��A�/A�RA��A^5A(�A�wA�PA�jA��A�AA�AȴA(�A��AVAbNA�-AS�A�A��AQ�A�FA��AO�A	+A �A�A�\Ax�A�!AQ�A �AA�;A��A`BA��Ax�A�;A 1@�"�@�
=@�K�@�@�E�@��7@��@���@��P@��H@��\@�7L@�j@�!@�?}@��/@�j@�@��m@�n�@���@�x�@�hs@�?}@�/@�j@�C�@�z�@睲@��H@�ff@��@�D@�+@�@�v�@�%@�Q�@���@���@۝�@�l�@�S�@�C�@�C�@�"�@��@��H@�ff@�E�@٩�@ؼj@�Q�@�1@ׅ@�n�@�O�@ԣ�@Ӆ@�
=@��H@ҸR@ҧ�@�V@�x�@�(�@��@·+@�@��T@͡�@�O�@��@���@̬@̬@̴9@�Ĝ@��`@��@�r�@�A�@�C�@�$�@ɡ�@�O�@��@�Ĝ@�r�@�Z@� �@��
@ǶF@ǥ�@Ǖ�@�l�@�S�@�33@�o@��@���@Ƨ�@�~�@ź^@Ĵ9@���@�K�@�ȴ@�n�@�=q@�-@�J@��T@���@�hs@��@���@�dZ@��H@�5?@�@��@�hs@�%@���@�j@� �@��@�~�@���@���@���@���@���@�x�@���@��@�I�@��m@�C�@�ȴ@�~�@�@�@�X@��/@��D@��@�z�@�r�@�bN@�Q�@�  @��;@��F@�S�@��@��+@�V@�M�@�$�@���@���@�&�@���@�r�@�b@��@��@�t�@�\)@�C�@�"�@�@�ȴ@�~�@�E�@�{@��^@�X@��@�%@��`@���@���@�r�@�Q�@�  @�ƨ@���@���@�t�@�\)@�;d@���@���@�V@���@��@�9X@�b@��;@���@�|�@��@��H@��R@��!@�^5@���@�r�@���@��F@�t�@�33@�o@��@�V@�-@�{@��#@��^@��7@�p�@�7L@���@��@��@�(�@��@�+@��@�o@���@��y@���@��R@�ff@���@�Ĝ@�+@��@���@���@�hs@�?}@���@�j@�b@���@�t�@���@�@���@��@���@�  @�dZ@���@�-@���@��/@��@�r�@�j@�bN@�Z@�Q�@�I�@�9X@�  @��T@���@�Ĝ@��@��u@��D@�1'@�K�@�@��T@��T@��T@��T@��T@��T@��#@��@��@��@��T@���@���@���@���@�Ĝ@��j@���@�I�@��@��@�ff@�$�@��h@�7L@��@��9@���@���@���@�r�@�A�@�(�@�(�@� �@�dZ@��+@��\@��@��@��@���@���@���@��w@��P@��@���@���@��
@���@���@���@���@��w@��@�;d@���@�V@��T@���@�?}@��/@���@���@�Q�@��@�;@�@l�@~�@~�+@~E�@~{@}�T@}@}��@}��@}�@}`B@}?}@|��@|��@zM�@y&�@x��@xĜ@x1'@w�@w+@v�R@v{@tz�@s@q�7@q&�@p�9@o�w@n�@nV@m��@m�h@l�@l�@l��@l�D@lZ@l1@k�F@kdZ@j~�@jJ@i��@i&�@h�u@g�;@gK�@g;d@g
=@f�y@f��@fv�@e��@e�@d�D@dI�@c��@c�F@c@b~�@a��@`�u@^$�@[��@Z��@Z^5@Xb@V@U�T@U�@UV@Tj@S�
@R=q@Q��@QX@QG�@Q7L@Q%@Q%@P�`@PbN@Pb@N�y@N��@N�R@NE�@M�@L�/@L��@L�@L9X@L1@L1@L(�@L�@K��@K�F@K��@K�@K�@KS�@KS�@KS�@K33@Ko@J�@J��@J�H@J��@J�@I�@Hr�@Hb@H  @H  @G�@G�@G�@G�;@G��@G�w@G�@G��@GK�@G;d@G;d@G;d@G+@G+@G�@G�@G+@G�@G�@G�@G�@G�@G
=@G
=@G�@G�@G+@G;d@G+@G�@G;d@G;d@G+@G�@G�@G�@G�@G
=@G�@F�@E`B@Ct�@Ct�@Ct�@C��@D�@Cƨ@A7L@?|�@?l�@?|�@?l�@?;d@?�@?�@>�y@>�y@>�@>ȴ@>v�@>ff@>E�@<j@;��@;�m@;�m@;�F@;dZ@;33@:�\@8�`@8��@8��@8��@8�`@8��@8�u@8�@8�@8�@8�@8�@8r�@8bN@8bN@8A�@8  @7�@7�w@7;d@6�R@6��@6�+@6ff@6@5@5��@5�h@5p�@4�@2��@3@3o@3@2�@1G�@1�@0��@0�9@1%@01'@/\)@.��@.v�@.@-��@-�@+�@*�@'|�@#�F@"��@"M�@!�@ Ĝ@ r�@�@ 1'@ �`@ ��@ 1'@   @ Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�"�A�&�A�1'A�33A�1'A�1'A�/A�/A�-A�-A�/A�33A�5?A�5?A�=qA�A�A�C�A�E�A�G�A�I�A�O�A�O�A�K�A�?}A�5?A�+A�"�A�{A�{A�VA���A��TA���A�A�A��Aۏ\Aڧ�A��A�  A��A�JA�bA��HAǕ�A��A��A�oA�`BA���A��A��A��A�Q�A�^5A�I�A��7A�&�A�JA��/A���A�C�A�l�A���A�M�A��A�VA�bNA���A��wA���A��A���A�ȴA�p�A�?}A���A��uA�oA�hsA���A��9A�z�A�M�A���A�{A�7LA��A���A��A�
=A�PA{S�AxȴAwG�At-ApbNAl-Ag�mAf�DAe&�A_t�A]O�AY�
AW&�AU�PAT1APv�ANZAL5?AJ�AH{ADr�AC��AA+A?�;A?VA<r�A7"�A5VA3hsA1�PA0�A/?}A.ffA,�uA,r�A,ZA,(�A,$�A, �A,1A+�A*r�A(1'A'�A%�A"�\A!�FA �9A�TAoA�A/A�A(�A��A�7At�AXAoAA(�A��A|�A`BA&�AVA��A�/A�RA��A^5A(�A�wA�PA�jA��A�AA�AȴA(�A��AVAbNA�-AS�A�A��AQ�A�FA��AO�A	+A �A�A�\Ax�A�!AQ�A �AA�;A��A`BA��Ax�A�;A 1@�"�@�
=@�K�@�@�E�@��7@��@���@��P@��H@��\@�7L@�j@�!@�?}@��/@�j@�@��m@�n�@���@�x�@�hs@�?}@�/@�j@�C�@�z�@睲@��H@�ff@��@�D@�+@�@�v�@�%@�Q�@���@���@۝�@�l�@�S�@�C�@�C�@�"�@��@��H@�ff@�E�@٩�@ؼj@�Q�@�1@ׅ@�n�@�O�@ԣ�@Ӆ@�
=@��H@ҸR@ҧ�@�V@�x�@�(�@��@·+@�@��T@͡�@�O�@��@���@̬@̬@̴9@�Ĝ@��`@��@�r�@�A�@�C�@�$�@ɡ�@�O�@��@�Ĝ@�r�@�Z@� �@��
@ǶF@ǥ�@Ǖ�@�l�@�S�@�33@�o@��@���@Ƨ�@�~�@ź^@Ĵ9@���@�K�@�ȴ@�n�@�=q@�-@�J@��T@���@�hs@��@���@�dZ@��H@�5?@�@��@�hs@�%@���@�j@� �@��@�~�@���@���@���@���@���@�x�@���@��@�I�@��m@�C�@�ȴ@�~�@�@�@�X@��/@��D@��@�z�@�r�@�bN@�Q�@�  @��;@��F@�S�@��@��+@�V@�M�@�$�@���@���@�&�@���@�r�@�b@��@��@�t�@�\)@�C�@�"�@�@�ȴ@�~�@�E�@�{@��^@�X@��@�%@��`@���@���@�r�@�Q�@�  @�ƨ@���@���@�t�@�\)@�;d@���@���@�V@���@��@�9X@�b@��;@���@�|�@��@��H@��R@��!@�^5@���@�r�@���@��F@�t�@�33@�o@��@�V@�-@�{@��#@��^@��7@�p�@�7L@���@��@��@�(�@��@�+@��@�o@���@��y@���@��R@�ff@���@�Ĝ@�+@��@���@���@�hs@�?}@���@�j@�b@���@�t�@���@�@���@��@���@�  @�dZ@���@�-@���@��/@��@�r�@�j@�bN@�Z@�Q�@�I�@�9X@�  @��T@���@�Ĝ@��@��u@��D@�1'@�K�@�@��T@��T@��T@��T@��T@��T@��#@��@��@��@��T@���@���@���@���@�Ĝ@��j@���@�I�@��@��@�ff@�$�@��h@�7L@��@��9@���@���@���@�r�@�A�@�(�@�(�@� �@�dZ@��+@��\@��@��@��@���@���@���@��w@��P@��@���@���@��
@���@���@���@���@��w@��@�;d@���@�V@��T@���@�?}@��/@���@���@�Q�@��@�;@�@l�@~�@~�+@~E�@~{@}�T@}@}��@}��@}�@}`B@}?}@|��@|��@zM�@y&�@x��@xĜ@x1'@w�@w+@v�R@v{@tz�@s@q�7@q&�@p�9@o�w@n�@nV@m��@m�h@l�@l�@l��@l�D@lZ@l1@k�F@kdZ@j~�@jJ@i��@i&�@h�u@g�;@gK�@g;d@g
=@f�y@f��@fv�@e��@e�@d�D@dI�@c��@c�F@c@b~�@a��@`�u@^$�@[��@Z��@Z^5@Xb@V@U�T@U�@UV@Tj@S�
@R=q@Q��@QX@QG�@Q7L@Q%@Q%@P�`@PbN@Pb@N�y@N��@N�R@NE�@M�@L�/@L��@L�@L9X@L1@L1@L(�@L�@K��@K�F@K��@K�@K�@KS�@KS�@KS�@K33@Ko@J�@J��@J�H@J��@J�@I�@Hr�@Hb@H  @H  @G�@G�@G�@G�;@G��@G�w@G�@G��@GK�@G;d@G;d@G;d@G+@G+@G�@G�@G+@G�@G�@G�@G�@G�@G
=@G
=@G�@G�@G+@G;d@G+@G�@G;d@G;d@G+@G�@G�@G�@G�@G
=@G�@F�@E`B@Ct�@Ct�@Ct�@C��@D�@Cƨ@A7L@?|�@?l�@?|�@?l�@?;d@?�@?�@>�y@>�y@>�@>ȴ@>v�@>ff@>E�@<j@;��@;�m@;�m@;�F@;dZ@;33@:�\@8�`@8��@8��@8��@8�`@8��@8�u@8�@8�@8�@8�@8�@8r�@8bN@8bN@8A�@8  @7�@7�w@7;d@6�R@6��@6�+@6ff@6@5@5��@5�h@5p�@4�@2��@3@3o@3@2�@1G�@1�@0��@0�9@1%@01'@/\)@.��@.v�@.@-��@-�@+�@*�@'|�@#�F@"��@"M�@!�@ Ĝ@ r�@�@ 1'@ �`@ ��@ 1'@   @ Q�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	8RB	9XB	;dB	<jB	:^B	:^B	9XB	8RB	8RB	7LB	8RB	9XB	:^B	9XB	=qB	?}B	@�B	A�B	A�B	D�B	K�B	W
B	[#B	aHB	gmB	iyB	jB	k�B	l�B	o�B	t�B	� B	�B	�}B	�dB	�B	�B	�wB	��B	�`B
B
PB
�B	��B
O�B
��B
�RB
ƨB
��B
�5B
��B
��B
��B
��B
�ZB
�mB  B�B@�BJ�BaHBYBgmBs�By�Bw�Bx�Bu�Bq�BaHB^5BW
BS�BM�BD�B<jB(�B �B1B
��B
��B
ǮB
ÖB
�^B
��B
hsB
VB
E�B
1'B
�B
B	�;B	�B	��B	�mB	�B	�qB	��B	�7B	}�B	l�B	\)B	XB	e`B	^5B	XB	D�B	6FB	+B	�B	{B��B�B�fB�/B�B��B��B�RB�XB�LB�^B�jBĜB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�TB�B�B��B	uB	{B	�B	�B	�B	#�B	.B	2-B	33B	49B	6FB	7LB	7LB	8RB	8RB	9XB	9XB	8RB	7LB	=qB	@�B	E�B	J�B	M�B	Q�B	ZB	`BB	cTB	k�B	p�B	v�B	w�B	w�B	x�B	z�B	y�B	z�B	z�B	}�B	}�B	�%B	�VB	�VB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	�uB	�=B	�B	�%B	�JB	�bB	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�'B	�'B	�'B	�?B	�^B	�qB	�qB	�qB	�wB	�wB	�wB	�wB	�qB	�qB	�qB	�qB	�jB	�^B	�XB	�RB	�^B	�XB	�XB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�RB	�LB	�RB	�LB	�RB	�XB	�XB	�XB	�^B	�^B	�dB	�^B	�dB	�dB	�dB	�qB	�qB	�}B	��B	B	�}B	��B	ÖB	ÖB	B	��B	��B	��B	��B	��B	B	ÖB	ĜB	ŢB	ǮB	ǮB	ɺB	ȴB	ȴB	ǮB	ǮB	ǮB	ȴB	ǮB	ȴB	ȴB	ȴB	ǮB	ȴB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�HB	�HB	�NB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�fB	�fB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�yB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
1B
1B
1B
1B
1B
1B
+B

=B
PB
JB
JB
\B
\B
\B
\B
hB
hB
hB
hB
uB
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
"�B
#�B
"�B
"�B
"�B
#�B
#�B
"�B
#�B
"�B
"�B
"�B
"�B
#�B
#�B
"�B
%�B
#�B
#�B
#�B
$�B
#�B
'�B
&�B
&�B
'�B
'�B
(�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
'�B
)�B
+B
(�B
(�B
+B
)�B
&�B
)�B
)�B
(�B
)�B
(�B
)�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
(�B
+B
)�B
)�B
)�B
)�B
+B
+B
)�B
+B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
-B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
6FB
7LB
6FB
6FB
8RB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
=qB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
C�B
E�B
D�B
D�B
I�B
H�B
I�B
H�B
I�B
J�B
J�B
K�B
K�B
L�B
L�B
L�B
L�B
K�B
L�B
L�B
M�B
N�B
M�B
M�B
M�B
M�B
O�B
N�B
N�B
P�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
O�B
P�B
P�B
R�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
Q�B
Q�B
Q�B
S�B
W
B
T�B
T�B
R�B
S�B
Q�B
W
B
W
B
W
B
W
B
W
B
XB
W
B
W
B
XB
XB
W
B
W
B
XB
W
B
W
B
[#B
YB
YB
YB
ZB
YB
ZB
\)B
\)B
\)B
[#B
[#B
\)B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
aHB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
aHB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
ffB
ffB
jB
o�B
m�B
m�B
o�B
o�B
p�B
q�B
o�B
n�B
o�B
p�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	8?B	95B	;^B	<wB	:cB	:lB	9]B	8cB	8SB	7JB	8EB	9XB	:cB	99B	=dB	?vB	@�B	A�B	A�B	D�B	K�B	W$B	[XB	arB	g�B	i�B	j�B	k�B	l�B	o�B	uB	�?B	��B	��B	��B	��B	��B	��B	�B	�*B
�B
�B
%tB
 *B
S�B
�iB
��B
��B
֔B
�%B
�B �B�B�B
��B
�B�BRBAMBO�Bl0B\FBi BvoB|�Bz�B|(By2Bx�BfMBf�BZ�BYsBV�BJ�BA�B3�B0�B�B
�jB
˽B
�eB
�B
�)B
�oB
l�B
\B
L%B
5�B
-�B
�B	�CB	��B	��B	�B	�VB	��B	��B	�>B	�B	q�B	e B	^�B	i�B	bhB	a�B	J�B	<sB	0�B	!�B	�B��B��B�B��BۀB�)B�B��B�UB��B�B�gB��B�4B�)B�^B��B��B�0B�B�VB�BӗBԡB�TB�+BհB�VB۩B��B�^B�B��B��B	�B	�B	�B	�B	B	(�B	/�B	2�B	3�B	4�B	6�B	7�B	7�B	8�B	8�B	:B	:B	9�B	=6B	?�B	@XB	E>B	J�B	M�B	R�B	\B	a�B	e;B	m�B	r�B	w�B	x�B	x�B	z�B	|�B	z`B	|LB	��B	�'B	|_B	��B	��B	��B	��B	�B	��B	�B	�xB	�bB	��B	�'B	��B	��B	��B	��B	��B	��B	��B	��B	�JB	�aB	�KB	��B	�vB	��B	�_B	��B	�YB	��B	�eB	�uB	�[B	�oB	��B	��B	��B	��B	��B	�_B	�B	�B	��B	��B	�NB	��B	��B	��B	��B	�[B	��B	��B	�B	��B	��B	��B	��B	�zB	�aB	��B	��B	��B	�>B	��B	��B	�B	�5B	�	B	��B	��B	��B	��B	��B	�VB	��B	��B	��B	�CB	�^B	�KB	ġB	��B	��B	��B	�'B	�7B	�B	��B	�B	��B	�B	�uB	�dB	İB	ƛB	�JB	��B	�B	ɿB	�gB	�rB	�B	�\B	��B	�8B	�PB	��B	��B	��B	�B	��B	�B	�B	�*B	��B	��B	�@B	�fB	��B	�lB	�AB	�B	ԭB	�^B	�(B	�BB	�^B	�rB	ԌB	ԦB	�/B	�YB	�"B	�VB	��B	גB	�SB	��B	ڥB	��B	��B	�=B	�xB	�EB	ߎB	�DB	�_B	ߒB	��B	�BB	�B	� B	�6B	�B	�cB	�B	�cB	�B	�WB	�gB	�B	�B	�B	�B	�B	�B	�B	��B	��B	�XB	�B	�0B	��B	�B	��B	��B	�B	�B	��B	�	B	�B	�pB	��B	��B	��B	��B	��B	��B	�-B	�GB	�,B	�&B	��B	��B	�AB	��B	��B	��B	�&B	�-B	�B	�wB	�MB	�B	��B	�B	�B	�0B	��B	��B	� B	��B	��B	�~B	�DB	�`B	�dB	�SB	��B	�fB	�EB	�B	��B	��B	��B	�B	��B	��B	��B	�WB	��B
 B
 eB
 KB
 �B
[B
|B
VB
�B
�B
�B
�B
B
�B
�B
]B
PB
oB
^B
oB
yB
�B
	�B
	WB
HB
�B
�B
@B
5B
�B
B
kB
(B
GB
�B
�B
�B
�B
7B
<B
�B
�B
B
jB
fB
gB
QB
�B
�B
�B
�B
�B
�B
�B
mB
�B
"�B
�B
�B
�B
�B
�B
!�B
%dB
$!B
"�B
"�B
"�B
#�B
#�B
"�B
#�B
"�B
"�B
"�B
#vB
#�B
$B
$0B
&lB
#�B
$)B
$�B
%�B
%�B
)4B
'�B
(BB
(�B
(�B
)zB
(B
( B
(B
)bB
)eB
)5B
)B
)RB
)�B
+�B
*�B
(�B
+B
*�B
'�B
%-B
)�B
)�B
)cB
)�B
(�B
)�B
(�B
(�B
)B
)'B
)�B
)�B
*vB
*xB
,�B
*�B
+7B
*�B
+B
+�B
+QB
*�B
+�B
,�B
,�B
,_B
,�B
,�B
,�B
-iB
-]B
-UB
-HB
-<B
- B
-AB
-DB
-NB
-�B
-�B
/�B
2}B
1tB
1�B
1�B
2�B
2�B
2�B
4CB
5.B
6#B
7�B
7�B
6�B
7wB
9bB
7�B
8�B
8�B
9B
9�B
9uB
9~B
9�B
9�B
9�B
9�B
;kB
;�B
;�B
<(B
=!B
=IB
>B
<�B
=�B
=�B
=�B
=�B
>cB
?B
? B
?�B
?�B
?�B
@LB
@!B
@>B
BIB
CHB
FDB
FyB
EhB
G2B
K�B
H�B
J�B
H�B
J�B
K�B
L�B
LSB
LNB
L�B
L�B
MB
L�B
LB
MwB
MiB
O!B
O)B
M�B
NuB
NoB
OB
PB
OB
OuB
Q)B
O�B
O�B
PB
P$B
P:B
PB
PB
O�B
P#B
O�B
O�B
PB
PB
PB
QB
P�B
PoB
Q�B
RNB
S�B
RtB
SB
Q�B
R
B
R�B
R�B
RB
SB
SB
SB
S*B
ShB
SB
R�B
S B
SB
R�B
SB
R�B
R�B
SB
R�B
R�B
R�B
SB
SB
R�B
R�B
R�B
R�B
R�B
SB
SB
R�B
R�B
SB
SB
R�B
R�B
SB
SB
R#B
R�B
T�B
V�B
WB
T�B
TfB
S(B
UHB
U�B
Y>B
W*B
WB
W9B
WZB
XIB
W#B
WSB
X!B
X:B
WCB
W�B
XVB
W�B
Y^B
[�B
Y;B
Y4B
YyB
Z�B
Y�B
[jB
^GB
\FB
\-B
[B
[FB
\VB
[�B
\FB
\3B
\3B
\7B
\6B
\LB
\JB
\;B
\iB
\�B
\XB
\�B
\�B
]�B
]VB
]lB
]|B
]�B
^�B
^jB
^cB
^�B
_�B
b�B
`B
`BB
`�B
aNB
b>B
a�B
a�B
b�B
b%B
bWB
cSB
d#B
c�B
c�B
c�B
deB
e_B
h�B
j_B
n�B
p�B
nnB
o"B
p#B
p&B
q7B
qBB
n�B
oB
p8B
p�B
oQB
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<K=�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<z�t<#�
<#�
<#�
<#�
<#�
<#�
<@Ų<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<=��<��#<��<#�
<#�
<#�
<#�
<#�
<wt<#�
<#�
<#�
<#�
<o�;<-F�<#�
<#�
<#�
<.`�<6�7<;��<#�
<#�
<q	<#�
<#�
<#�
<#�
<#�
<-~�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<E@�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.08 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  202007211526532020072115265320200721152653  AO  ARCAADJP                                                                    20191217100045    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191217100045  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191217100045  QCF$                G�O�G�O�G�O�0               PM  ARSQPADJV1.1                                                                20200721152653  QC  PRES            @���Dǣ3G�O�                PM  ARSQCTM V1.1                                                                20200721152653  QC  PSAL            @���Dǣ3G�O�                PM  ARSQCOWGV1.1CTD_2018v2 + Argo_2018v01                                       20200901151527  IP                  G�O�G�O�G�O�                