CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:07:10Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       BD   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IT   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  b   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20181024140710  20181024140710  5904955 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               %A   AO  6558                            2B  A   APEX                            7469                            062512                          846 @׸�����1   @׸���@3��t�j�c��E��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      %A   B   B   @,��@�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D'��D(y�D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/�fD0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dy�D�J=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@'�@���@���@�p�A�RA>�RA^�RA~�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\)B�BG�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��
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
B��
C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D z�D ��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D	z�D	��D
z�D
��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��Dz�D��D z�D ��D!z�D!��D"z�D"��D#z�D#��D$z�D$��D%z�D%��D&z�D&��D'z�D'�{D(t{D(��D)z�D)��D*z�D*��D+z�D+��D,z�D,��D-z�D-��D.z�D/GD/�GD0GD0z�D0��D1z�D1��D2z�D2��D3z�D3��D4z�D4��D5z�D5��D6z�D6��D7z�D7��D8z�D8��D9z�D9��D:z�D:��D;z�D;��D<z�D<��D=z�D=��D>z�D>��D?z�D?��D@z�D@��DAz�DA��DBz�DB��DCz�DC��DDz�DD��DEz�DE��DFz�DF��DGz�DG��DHz�DH��DIz�DI��DJz�DJ��DKz�DK��DLz�DL��DMz�DM��DNz�DN��DOz�DO��DPz�D\z�D\��D]z�D]��D^z�D^��D_z�D_��D`z�D`��Daz�Da��Dbz�Db��Dcz�Dc��Ddz�Dd��Dez�De��Dfz�Df��Dgz�Dg��Dhz�Dh��Diz�Di��Djz�Dj��Dkz�Dk��Dlz�Dl��Dmz�Dm��Dnz�Dn��Doz�Do��Dpz�Dp��Dqz�Dq��Drz�Dr��Dsz�Ds��Dtz�Dt��Duz�Du��Dvz�Dv��Dwz�DwǮDy� D�G�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�I�A�G�A�K�A�I�A�C�A�G�A�I�A�O�A�XA�XA�VA�S�A�XA�9XA�1'A׸RA�jA�33AҍPA��yA�ĜAѥ�A�v�A�M�A�bA���A���A���A�G�A�p�A�|�A�ffAʧ�A�ĜA�`BA�p�A��A��yA�x�A�;dA���A�bNA�JA�+A��A��/A�I�A���A�l�A�A��
A�S�A�  A��`A�
=A��A��jA�I�A�v�A���A��A��7A���A��DA��`A�ffA��#A���A�C�A��jA��A��A��-A��A���A��A���A�bNA��7A���A�z�A� �A�9XA�l�A���A��A�t�A�ĜA��A��A��FA���A�%A���A���A�dZA��RA��mA�  A���A��A���A��uA�oA��A}��A{��Aw�FAs�Ar-Ap��An�Am%AioAfv�Ac�;AbjA^�uA[p�AW33AVI�AU��AUoARA�APZAO�7AN��AL�uAKAJ�DAI`BAGl�AE�mAC��AA�A?��A=�-A;�-A8M�A61A3�^A0�`A/��A.��A-��A,r�A*�HA*ĜA)\)A(5?A'�hA'K�A&��A%�A%/A$M�A"�A"E�A ��Az�Ax�A�A��AE�A&�A5?A;dA�9Az�A��A�AQ�A;dAz�AC�AȴA��A�DA�!A�A�A��A��A	��A��A�A��A�7A~�AjAI�A{A�A�A�A��AdZA��AdZ@�C�@��@�
=@�v�@���@�r�@��@���@��@�  @�ƨ@�t�@�"�@�v�@���@ץ�@֗�@�{@պ^@Ձ@�%@�I�@��
@�o@ҏ\@ёh@У�@�
=@�@̬@��
@��@ɺ^@���@�(�@�ƨ@Ƨ�@�5?@�?}@��`@ċD@�1'@å�@��@�bN@�b@þw@�C�@�~�@���@���@�S�@�+@�@��H@��@�`B@��D@�"�@�l�@�33@���@�n�@�5?@��#@�V@�Z@��;@�S�@��@�ff@�=q@�$�@��@��@��u@�A�@���@�l�@�n�@��@�p�@�?}@���@���@�r�@�Q�@�b@���@��@�\)@�33@��@���@�V@��^@��h@�7L@��@��9@�1@���@�33@�ȴ@���@�x�@�/@��@��@��@��@�%@�?}@��@�ff@��H@��+@�{@��T@��^@���@��h@��7@�`B@�G�@��@��@��`@���@�bN@�b@��@�I�@�1@���@�|�@�v�@�p�@��+@��H@��H@��R@�ff@���@��/@�Z@�7L@��`@��@�j@��D@��H@�V@���@��m@�b@���@�;d@��H@�v�@��@�$�@�5?@�-@��@��-@���@��T@��T@��h@�&�@���@��@��@��@�9X@� �@�1@�1@��D@���@�{@��@��y@�ff@�x�@�7L@��j@��F@��@���@��@�|�@�l�@�\)@���@�S�@�J@�p�@�?}@�&�@���@���@��/@��9@�z�@�z�@�Z@�ƨ@�K�@�"�@��@�;d@�t�@��F@�|�@�dZ@�C�@��!@�5?@�-@��@��^@�hs@���@��@���@��
@���@�|�@�o@��R@�n�@�M�@�=q@��@��j@�1'@� �@� �@��@�b@�b@�1@��@��F@���@�\)@�;d@�+@�"�@��@tl"11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�I�A�G�A�K�A�I�A�C�A�G�A�I�A�O�A�XA�XA�VA�S�A�XA�9XA�1'A׸RA�jA�33AҍPA��yA�ĜAѥ�A�v�A�M�A�bA���A���A���A�G�A�p�A�|�A�ffAʧ�A�ĜA�`BA�p�A��A��yA�x�A�;dA���A�bNA�JA�+A��A��/A�I�A���A�l�A�A��
A�S�A�  A��`A�
=A��A��jA�I�A�v�A���A��A��7A���A��DA��`A�ffA��#A���A�C�A��jA��A��A��-A��A���A��A���A�bNA��7A���A�z�A� �A�9XA�l�A���A��A�t�A�ĜA��A��A��FA���A�%A���A���A�dZA��RA��mA�  A���A��A���A��uA�oA��A}��A{��Aw�FAs�Ar-Ap��An�Am%AioAfv�Ac�;AbjA^�uA[p�AW33AVI�AU��AUoARA�APZAO�7AN��AL�uAKAJ�DAI`BAGl�AE�mAC��AA�A?��A=�-A;�-A8M�A61A3�^A0�`A/��A.��A-��A,r�A*�HA*ĜA)\)A(5?A'�hA'K�A&��A%�A%/A$M�A"�A"E�A ��Az�Ax�A�A��AE�A&�A5?A;dA�9Az�A��A�AQ�A;dAz�AC�AȴA��A�DA�!A�A�A��A��A	��A��A�A��A�7A~�AjAI�A{A�A�A�A��AdZA��AdZ@�C�@��@�
=@�v�@���@�r�@��@���@��@�  @�ƨ@�t�@�"�@�v�@���@ץ�@֗�@�{@պ^@Ձ@�%@�I�@��
@�o@ҏ\@ёh@У�@�
=@�@̬@��
@��@ɺ^@���@�(�@�ƨ@Ƨ�@�5?@�?}@��`@ċD@�1'@å�@��@�bN@�b@þw@�C�@�~�@���@���@�S�@�+@�@��H@��@�`B@��D@�"�@�l�@�33@���@�n�@�5?@��#@�V@�Z@��;@�S�@��@�ff@�=q@�$�@��@��@��u@�A�@���@�l�@�n�@��@�p�@�?}@���@���@�r�@�Q�@�b@���@��@�\)@�33@��@���@�V@��^@��h@�7L@��@��9@�1@���@�33@�ȴ@���@�x�@�/@��@��@��@��@�%@�?}@��@�ff@��H@��+@�{@��T@��^@���@��h@��7@�`B@�G�@��@��@��`@���@�bN@�b@��@�I�@�1@���@�|�@�v�@�p�@��+@��H@��H@��R@�ff@���@��/@�Z@�7L@��`@��@�j@��D@��H@�V@���@��m@�b@���@�;d@��H@�v�@��@�$�@�5?@�-@��@��-@���@��T@��T@��h@�&�@���@��@��@��@�9X@� �@�1@�1@��D@���@�{@��@��y@�ff@�x�@�7L@��j@��F@��@���@��@�|�@�l�@�\)@���@�S�@�J@�p�@�?}@�&�@���@���@��/@��9@�z�@�z�@�Z@�ƨ@�K�@�"�@��@�;d@�t�@��F@�|�@�dZ@�C�@��!@�5?@�-@��@��^@�hs@���@��@���@��
@���@�|�@�o@��R@�n�@�M�@�=q@��@��j@�1'@� �@� �@��@�b@�b@�1@��@��F@���@�\)@�;d@�+@�"�@��@tl"11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B=qB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB=qB?}B>wBB�BaHBo�B�+B�oB�{B�{B��B��B��B��B��B��B��B�B�9B��BɺB��B�B��B�B-B.B8RB9XB;dB<jBD�BL�BT�B]/BcTBjBp�Bp�Bt�Bv�B{�B�B�1B��B��B�{B��B��B�B�jB�RB�dB�dB�LB�FB�LB�?B�B��B�hB�DB�Bx�Bk�BXBJ�B;dB�B  B�B�fB�BŢB��B�BR�BD�B6FB#�BB
�HB
��B
ȴB
��B
�9B
��B
��B
�JB
{�B
\)B
H�B
<jB
)�B
�B
  B	�sB	�#B	��B	ÖB	�RB	�B	��B	��B	�{B	}�B	l�B	XB	P�B	M�B	C�B	(�B	�B	�B	hB	VB	1B	  B��B�yB�;B�B��B��B��BǮB�?B��B��B��B�\B�JB�1B�+B�1B�+B�JB�DB�=B�7B�1B�+B�B�B~�B|�B{�By�Bz�B|�B}�B}�B|�B{�Bz�Bz�By�By�Bz�By�Bx�Bl�BffBe`BhsBk�Bp�Bp�BiyBaHB[#BT�BQ�BP�BO�BP�BR�BT�BW
BYB[#B\)B^5BbNBffBk�Bp�Bo�Bm�Bn�Bs�B�+B�%B�JB�hB��B��B��B��B��B��BE�B��B�B�B�B�B�B�9B�XB�qB�wB�}B�wB�qB�wB�}BÖBƨBȴBɺB��B��B��B��B�B�B�#B�/B�HB�B��B��B��B��B��B	B	+B	1B	1B	1B	1B	JB	\B	bB	bB	{B	�B	�B	�B	�B	 �B	$�B	#�B	(�B	-B	-B	/B	0!B	0!B	5?B	9XB	;dB	<jB	>wB	?}B	@�B	@�B	C�B	D�B	E�B	G�B	H�B	J�B	K�B	N�B	O�B	Q�B	Q�B	R�B	S�B	XB	\)B	]/B	^5B	_;B	`BB	cTB	e`B	gmB	jB	o�B	q�B	t�B	t�B	t�B	t�B	u�B	w�B	{�B	�B	�1B	�VB	�bB	�\B	�\B	�bB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�3B	�-B	�B	�B	�LB	�XB	�^B	�jB	�dB	�FB	�B	�B	�FB	�RB	�RB	�RB	�^B	�qB	�}B	ŢB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�/B	�;B	�HB	�NB	�NB	�NB	�HB	�HB	�5B	�B	�
B	�B	�
B	�
B	�B	�/B	�NB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
%B
1B
1B
1B
+B
+B
	7B
	7B

=B

=B
	7B
+B
1B

=B
DB
DB
	7B
1B
1B
+B
+B
+B
+B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
PB
VB
hB
!�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B=qB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB=qB?}B>wBB�BaHBo�B�+B�oB�{B�{B��B��B��B��B��B��B��B�B�9B��BɺB��B�B��B�B-B.B8RB9XB;dB<jBD�BL�BT�B]/BcTBjBp�Bp�Bt�Bv�B{�B�B�1B��B��B�{B��B��B�B�jB�RB�dB�dB�LB�FB�LB�?B�B��B�hB�DB�Bx�Bk�BXBJ�B;dB�B  B�B�fB�BŢB��B�BR�BD�B6FB#�BB
�HB
��B
ȴB
��B
�9B
��B
��B
�JB
{�B
\)B
H�B
<jB
)�B
�B
  B	�sB	�#B	��B	ÖB	�RB	�B	��B	��B	�{B	}�B	l�B	XB	P�B	M�B	C�B	(�B	�B	�B	hB	VB	1B	  B��B�yB�;B�B��B��B��BǮB�?B��B��B��B�\B�JB�1B�+B�1B�+B�JB�DB�=B�7B�1B�+B�B�B~�B|�B{�By�Bz�B|�B}�B}�B|�B{�Bz�Bz�By�By�Bz�By�Bx�Bl�BffBe`BhsBk�Bp�Bp�BiyBaHB[#BT�BQ�BP�BO�BP�BR�BT�BW
BYB[#B\)B^5BbNBffBk�Bp�Bo�Bm�Bn�Bs�B�+B�%B�JB�hB��B��B��B��B��B��BE�B��B�B�B�B�B�B�9B�XB�qB�wB�}B�wB�qB�wB�}BÖBƨBȴBɺB��B��B��B��B�B�B�#B�/B�HB�B��B��B��B��B��B	B	+B	1B	1B	1B	1B	JB	\B	bB	bB	{B	�B	�B	�B	�B	 �B	$�B	#�B	(�B	-B	-B	/B	0!B	0!B	5?B	9XB	;dB	<jB	>wB	?}B	@�B	@�B	C�B	D�B	E�B	G�B	H�B	J�B	K�B	N�B	O�B	Q�B	Q�B	R�B	S�B	XB	\)B	]/B	^5B	_;B	`BB	cTB	e`B	gmB	jB	o�B	q�B	t�B	t�B	t�B	t�B	u�B	w�B	{�B	�B	�1B	�VB	�bB	�\B	�\B	�bB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�3B	�-B	�B	�B	�LB	�XB	�^B	�jB	�dB	�FB	�B	�B	�FB	�RB	�RB	�RB	�^B	�qB	�}B	ŢB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�/B	�;B	�HB	�NB	�NB	�NB	�HB	�HB	�5B	�B	�
B	�B	�
B	�
B	�B	�/B	�NB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
%B
1B
1B
1B
+B
+B
	7B
	7B

=B

=B
	7B
+B
1B

=B
DB
DB
	7B
1B
1B
+B
+B
+B
+B
	7B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
PB
VB
hB
!�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140710                              AO  ARCAADJP                                                                    20181024140710    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140710  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140710  QCF$                G�O�G�O�G�O�4000            