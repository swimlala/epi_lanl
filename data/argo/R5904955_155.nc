CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:07:35Z creation      
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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140735  20181024140735  5904955 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6558                            2B  A   APEX                            7469                            062512                          846 @��%�S�1   @��%��8�@5&�-�cƧ-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @9��@y��@�  A   A   A@  A`  A�  A���A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw� Dy��D�I�D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @4z�@tz�@�p�@�p�A�RA>�RA^�RA~�RA�(�A�\)A�\)A�\)A�\)A�\)A�\)A��\B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
B��
B��
B��
B��
B�
=B���B��
B��
B��
B��
B��
B��
B��
B��
B�
=B�=pBǣ�B��
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
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA��CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'��D(z�D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D.��D/z�D/��D0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�DP��DQz�DQ��DRz�DR��DSz�DS��DTz�DT��DUz�DU��DVz�DV��DWz�DW��DXz�DX��DYz�DY��DZz�DZ��D[z�D[��D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�Dw��Dy}pD�G]D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�A�p�A�|�AځAڅAڃAڅAڅAڇ+Aڇ+A�|�AڅA�~�AځA�z�A�|�AڅAڇ+AڋDAړuAڕ�Aڙ�Aڕ�AڋDAڋDAڍPAڏ\Aڏ\AڑhAڑhAڕ�Aڕ�Aڗ�AړuAړuAڍPAڏ\Aڙ�AځA��A�AնFA�=qA��/A�1Aͣ�A˧�A�bNA�-A���AȾwA�bA�z�AĶFA�A�A��A���A�p�A�oA�r�A���A�ȴA�~�A��A�t�A�^5A�?}A�bNA�9XA���A���A���A��\A��HA�^5A��A��hA��A���A�Q�A�\)A�hsA�A�bA���A��A��A���A��uA���A��A���A�bNA�(�A�oA��A��9A��uA�x�A�E�A�  A���A�ĜA��;A�33A�1'A�=qA��A�t�A�C�A�1'A�A���A��PA�=qA��A��A~��A}?}Azr�AyO�Ax�!AtZAmhsAk�TAj�Af�+Ad9XAaƨA^�HA\�`A\I�A[l�AWXAS?}AP�HAO/ALĜAJ�AJ��AJ�DAJ(�AH$�AF��AFv�AE�;AE`BAD�+AA�AA
=A@�A?�A?��A?oA<E�A7��A4ȴA4E�A3��A3O�A2�RA1��A1�A0A-�mA,��A+��A*=qA(��A%�;A%t�A$�yA#G�A!�A ��AA
=A+A$�A/A�A9XA�TA�PAA��A�A�9A�AjA��A7LA�/A^5A�#A|�AoA^5A�A��A�DAVA
�\A;dA-A\)A^5A�uA�AȴAQ�A��A��A\)A33A�Al�A I�@��@�x�@�Z@� �@���@��@�S�@�M�@�/@�P@�ff@��@��@��@�\@�E�@�@�O�@�j@�\)@��@�E�@�@��@�@畁@��@��m@�@���@�x�@�j@޸R@ܴ9@ە�@ڧ�@��@�X@�z�@�
=@��@Լj@��@ҟ�@с@��
@�;d@�@���@Ώ\@���@�G�@�ƨ@�@��m@�C�@��@�E�@ũ�@őh@�`B@��@��/@ċD@��@��;@þw@�l�@°!@���@��D@��@�ȴ@�V@��T@���@�X@���@��u@�9X@��@��@�ff@�^5@��@�x�@�&�@��`@�A�@� �@� �@��@�1@��m@�ƨ@�K�@�
=@���@���@�~�@�p�@��`@��@�j@�9X@�b@�ƨ@�|�@�S�@��@�ff@��@���@��^@���@���@���@���@�O�@�%@���@���@��D@�j@��@�S�@��@��@�J@��@��^@���@���@�Q�@�1@��P@�
=@�M�@���@�7L@���@���@��@�;d@��\@�M�@�{@���@���@���@��7@��h@�X@��@���@�r�@�A�@��m@�l�@�o@���@��y@�~�@��@��#@��-@��@�hs@�?}@�/@���@���@��@�t�@�t�@���@�|�@�C�@�;d@�33@�"�@��y@�5?@�/@�V@��@�Ĝ@��@��@��y@��\@�^5@�$�@��^@�`B@�?}@���@��j@��j@��@� �@���@��@�dZ@�\)@�K�@�33@��H@��!@���@�$�@�p�@�`B@�`B@�X@�X@�?}@���@���@��u@��@�j@�Z@�Z@�Q�@�I�@�9X@��@�ƨ@�ƨ@�ƨ@��w@�+@�E�@��#@���@���@��^@�x�@��@�%@��/@�I�@�  @�|�@��@�$�@��#@���@��-@�X@���@��@��@��@��@��w@��F@�ƨ@���@�dZ@���@���@���@�v�@��@��-@��@�?}@�%@��`@�r�@���@��m@��
@���@���@��F@��!@w�a@e�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�p�A�|�AځAڅAڃAڅAڅAڇ+Aڇ+A�|�AڅA�~�AځA�z�A�|�AڅAڇ+AڋDAړuAڕ�Aڙ�Aڕ�AڋDAڋDAڍPAڏ\Aڏ\AڑhAڑhAڕ�Aڕ�Aڗ�AړuAړuAڍPAڏ\Aڙ�AځA��A�AնFA�=qA��/A�1Aͣ�A˧�A�bNA�-A���AȾwA�bA�z�AĶFA�A�A��A���A�p�A�oA�r�A���A�ȴA�~�A��A�t�A�^5A�?}A�bNA�9XA���A���A���A��\A��HA�^5A��A��hA��A���A�Q�A�\)A�hsA�A�bA���A��A��A���A��uA���A��A���A�bNA�(�A�oA��A��9A��uA�x�A�E�A�  A���A�ĜA��;A�33A�1'A�=qA��A�t�A�C�A�1'A�A���A��PA�=qA��A��A~��A}?}Azr�AyO�Ax�!AtZAmhsAk�TAj�Af�+Ad9XAaƨA^�HA\�`A\I�A[l�AWXAS?}AP�HAO/ALĜAJ�AJ��AJ�DAJ(�AH$�AF��AFv�AE�;AE`BAD�+AA�AA
=A@�A?�A?��A?oA<E�A7��A4ȴA4E�A3��A3O�A2�RA1��A1�A0A-�mA,��A+��A*=qA(��A%�;A%t�A$�yA#G�A!�A ��AA
=A+A$�A/A�A9XA�TA�PAA��A�A�9A�AjA��A7LA�/A^5A�#A|�AoA^5A�A��A�DAVA
�\A;dA-A\)A^5A�uA�AȴAQ�A��A��A\)A33A�Al�A I�@��@�x�@�Z@� �@���@��@�S�@�M�@�/@�P@�ff@��@��@��@�\@�E�@�@�O�@�j@�\)@��@�E�@�@��@�@畁@��@��m@�@���@�x�@�j@޸R@ܴ9@ە�@ڧ�@��@�X@�z�@�
=@��@Լj@��@ҟ�@с@��
@�;d@�@���@Ώ\@���@�G�@�ƨ@�@��m@�C�@��@�E�@ũ�@őh@�`B@��@��/@ċD@��@��;@þw@�l�@°!@���@��D@��@�ȴ@�V@��T@���@�X@���@��u@�9X@��@��@�ff@�^5@��@�x�@�&�@��`@�A�@� �@� �@��@�1@��m@�ƨ@�K�@�
=@���@���@�~�@�p�@��`@��@�j@�9X@�b@�ƨ@�|�@�S�@��@�ff@��@���@��^@���@���@���@���@�O�@�%@���@���@��D@�j@��@�S�@��@��@�J@��@��^@���@���@�Q�@�1@��P@�
=@�M�@���@�7L@���@���@��@�;d@��\@�M�@�{@���@���@���@��7@��h@�X@��@���@�r�@�A�@��m@�l�@�o@���@��y@�~�@��@��#@��-@��@�hs@�?}@�/@���@���@��@�t�@�t�@���@�|�@�C�@�;d@�33@�"�@��y@�5?@�/@�V@��@�Ĝ@��@��@��y@��\@�^5@�$�@��^@�`B@�?}@���@��j@��j@��@� �@���@��@�dZ@�\)@�K�@�33@��H@��!@���@�$�@�p�@�`B@�`B@�X@�X@�?}@���@���@��u@��@�j@�Z@�Z@�Q�@�I�@�9X@��@�ƨ@�ƨ@�ƨ@��w@�+@�E�@��#@���@���@��^@�x�@��@�%@��/@�I�@�  @�|�@��@�$�@��#@���@��-@�X@���@��@��@��@��@��w@��F@�ƨ@���@�dZ@���@���@���@�v�@��@��-@��@�?}@�%@��`@�r�@���@��m@��
@���@���@��F@��!@w�a@e�91111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BL�BL�BK�BK�BK�BL�BK�BL�BL�BL�BK�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BK�BK�BL�BK�BK�BK�BL�BL�BL�BL�BL�BL�BL�BL�BL�BJ�BbNBx�B�+B�{B��B�\B��B��B��B�B�NB��BDBuB8RB@�BB�B`BBl�Bv�B�JB�DB�B� B|�Bw�Bm�BffBk�BjBgmBaHBZB\)B^5B`BBdZBdZBdZBaHBZBQ�BH�BC�B:^B+B�B��B�sB�B��B��B��B��B�B�)B�BB��B�DB�+B�Bw�B^5BD�B-B{B
��B
�HB
��B
�LB
��B
��B
�B
�B
|�B
u�B
`BB
=qB
0!B
�B
�B
PB	�B	��B	�9B	�B	��B	�7B	x�B	k�B	aHB	\)B	T�B	=qB	)�B	�B	uB	DB	B	B	B��B��B�B�B�B�B�mB�HB�/B�)B�B�B��BǮB�RB�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�VB�DB�=B�1B�1B�+B�B�%B�%B�%B�B�JB�B�B�B�B� B� B�B�B|�Bx�Bz�Bx�Bw�Bw�Bv�Bl�B^5B[#BZB\)Be`Bp�Bu�B{�Bz�By�Bz�B|�B|�Bx�Bq�BiyBiyBhsBhsBhsBhsBhsBjBm�Bq�Br�Br�Bs�Bs�Bt�Bt�Bt�Bu�Bu�Bu�Bv�Bw�By�B}�B�B�B�B�%B�7B�1B�=B�VB�uB��B��B��B��B��B��B��B��B��B��B�B�RB�jB�dB�^B�^B�^B�jB��BǮB��B��B��B��B��B�B�
B�
B�B�B�B�B�B�#B�#B�)B�5B�TB�sB�B�B�B�B�B��B��B��B��B	B	B	B	B	B	+B	1B	JB	JB	JB	JB	PB	PB	VB	uB	�B	�B	�B	�B	�B	"�B	#�B	&�B	'�B	(�B	+B	-B	.B	0!B	33B	6FB	7LB	8RB	9XB	9XB	9XB	:^B	<jB	>wB	@�B	B�B	C�B	C�B	G�B	K�B	L�B	M�B	S�B	T�B	T�B	YB	\)B	^5B	aHB	cTB	ffB	k�B	p�B	t�B	u�B	u�B	z�B	{�B	{�B	|�B	}�B	}�B	~�B	�B	�B	�B	�B	�B	�+B	�7B	�7B	�7B	�DB	�PB	�VB	�VB	�bB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�?B	�?B	�?B	�?B	�9B	�?B	�?B	�?B	�FB	�LB	�RB	�XB	�dB	�jB	�qB	��B	��B	��B	��B	��B	B	B	B	ÖB	ÖB	ÖB	ĜB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�)B	�#B	�/B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�fB	�fB	�`B	�ZB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
  B
B
 �B
�B
"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BL�BL�BK�BK�BK�BL�BK�BL�BL�BL�BK�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BL�BK�BK�BL�BK�BK�BK�BL�BL�BL�BL�BL�BL�BL�BL�BL�BJ�BbNBx�B�+B�{B��B�\B��B��B��B�B�NB��BDBuB8RB@�BB�B`BBl�Bv�B�JB�DB�B� B|�Bw�Bm�BffBk�BjBgmBaHBZB\)B^5B`BBdZBdZBdZBaHBZBQ�BH�BC�B:^B+B�B��B�sB�B��B��B��B��B�B�)B�BB��B�DB�+B�Bw�B^5BD�B-B{B
��B
�HB
��B
�LB
��B
��B
�B
�B
|�B
u�B
`BB
=qB
0!B
�B
�B
PB	�B	��B	�9B	�B	��B	�7B	x�B	k�B	aHB	\)B	T�B	=qB	)�B	�B	uB	DB	B	B	B��B��B�B�B�B�B�mB�HB�/B�)B�B�B��BǮB�RB�!B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�VB�DB�=B�1B�1B�+B�B�%B�%B�%B�B�JB�B�B�B�B� B� B�B�B|�Bx�Bz�Bx�Bw�Bw�Bv�Bl�B^5B[#BZB\)Be`Bp�Bu�B{�Bz�By�Bz�B|�B|�Bx�Bq�BiyBiyBhsBhsBhsBhsBhsBjBm�Bq�Br�Br�Bs�Bs�Bt�Bt�Bt�Bu�Bu�Bu�Bv�Bw�By�B}�B�B�B�B�%B�7B�1B�=B�VB�uB��B��B��B��B��B��B��B��B��B��B�B�RB�jB�dB�^B�^B�^B�jB��BǮB��B��B��B��B��B�B�
B�
B�B�B�B�B�B�#B�#B�)B�5B�TB�sB�B�B�B�B�B��B��B��B��B	B	B	B	B	B	+B	1B	JB	JB	JB	JB	PB	PB	VB	uB	�B	�B	�B	�B	�B	"�B	#�B	&�B	'�B	(�B	+B	-B	.B	0!B	33B	6FB	7LB	8RB	9XB	9XB	9XB	:^B	<jB	>wB	@�B	B�B	C�B	C�B	G�B	K�B	L�B	M�B	S�B	T�B	T�B	YB	\)B	^5B	aHB	cTB	ffB	k�B	p�B	t�B	u�B	u�B	z�B	{�B	{�B	|�B	}�B	}�B	~�B	�B	�B	�B	�B	�B	�+B	�7B	�7B	�7B	�DB	�PB	�VB	�VB	�bB	�uB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�?B	�?B	�?B	�?B	�9B	�?B	�?B	�?B	�FB	�LB	�RB	�XB	�dB	�jB	�qB	��B	��B	��B	��B	��B	B	B	B	ÖB	ÖB	ÖB	ĜB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�)B	�)B	�)B	�)B	�#B	�/B	�;B	�BB	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�fB	�fB	�`B	�ZB	�ZB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
  B
B
 �B
�B
"�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140735                              AO  ARCAADJP                                                                    20181024140735    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140735  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140735  QCF$                G�O�G�O�G�O�0               