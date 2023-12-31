CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2020-05-24T00:37:45Z creation;2020-05-24T00:37:47Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `(   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �D   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  �x   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20200524003745  20200524005552  1902338                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   NAVIS_A                         0922                            ARGO                            863 @����1   @����-��DI�^@D�l�C��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A>ffA`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DPy�DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� DtfDt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�FfD�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@���@���Az�A:�GA\z�A|z�A�=qA�=qA�p�A�=qA�=qA�=qA�=qA�=qB�B�B�B�B'�B/�B7�B>�RBG�BN�RBW�B_�Bg�Bo�Bw�B�B��\B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B��\CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C��
C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPk�DP��DQq�DQ��DRq�DR��DSq�DS��DTxRDT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds�RDtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�?\D�\)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Ao7LAoG�AoK�AoO�AoS�AoS�AoXAo\)Ao`BAo\)Ao`BAodZAol�Aol�Ao|�Ao�7Ao��ApE�Ap�Aq��Aq��Aq��Aq��Aq�hAq�7Aq�Aq�PAq��Aq��Aq��Aq��Aq�PAq�7Aq�PAq�hAq��Aq��Aq��Aq�Aq�FAq�^Aq�^Aq�^Aq�^Aq�^Aq�^Aq�FAq�FAq�-Aq��Aq|�Ap�ApA�Al1Ag|�Ae%Ae;dAg&�Adn�Aa|�A`�+A_+A^ �A\z�AZ$�AWVAU��AS��AQ�AQ7LAP�APbNANI�AM�hAM;dAL��AL�yAL�9ALI�AL �AK��AJ��AJ�AJE�AI��AIdZAHn�AF��AE7LADv�ADAC��AB1AAXAAoA@��A@�DA@Q�A@�A?�#A?��A?�A?x�A?�A>r�A>Q�A>�+A>{A=�A;��A:bA933A8-A7��A6��A6$�A5�A5C�A4v�A4A�A3�mA3ƨA3�TA3p�A2Q�A2bA1ƨA1�7A1G�A0�`A0��A0�RA0jA0(�A/oA.$�A-��A-��A-
=A,Q�A+�;A+7LA*��A*(�A)K�A(�yA(�\A(5?A'�;A'�#A&�/A&�+A&��A&�HA&��A&A�A%�-A%��A%p�A%%A$��A#A#oA"�A"�HA"ȴA"��A#+A#;dA#7LA#+A#oA#��A$bNA$�DA$v�A$�+A$�+A$M�A$-A$�A$bA$A#�A#�-A"�A!��A!��A"VA"5?A!�;A!`BA!oA �9A ^5A (�A 1A��A��A�A��Az�A��Ax�AS�A�A1'A�;A;dA��A�uA1A��A�FAdZAȴAE�A1A�^A"�A�AffAG�A��A~�A�A�A��A�9An�AQ�AA�A$�A�wA�A�\AI�A�AS�A��AZA��A��A|�A�AffA��A�;A�hA�A
�yA
�/A
��A
bNA
JA	�#A	��A	7LAĜA�At�A33A&�AoA�#AdZAC�A�/A�!Az�AbNA1'A�PA �AC�A Ĝ@�t�@��@�7L@���@�K�@���@��T@��@�+@�5?@�?}@�!@�@�t�@���@�j@��@�@�Z@��@��@�E�@�J@�h@�?}@���@��@��@�n�@�X@ܣ�@���@��y@�ff@��@��@ڗ�@�ƨ@�b@���@�^5@�o@ҟ�@�E�@�1'@�;d@��@ə�@�j@�n�@��#@��
@�@��@�~�@\@��
@��/@�p�@��#@���@�X@Ĵ9@�o@��@��`@�9X@��@�|�@���@�/@�r�@�
=@�^5@�V@�5?@��^@��`@�r�@��@�J@�&�@��@�9X@��@��R@���@��#@���@�G�@��@��u@�(�@��
@�S�@���@�E�@�@��^@�V@���@�ƨ@�t�@���@�-@��@�@��@�G�@��@�1'@��F@���@�=q@�$�@�{@�J@�@��@�G�@�%@��@�I�@���@�C�@��@���@���@��\@�-@���@���@�(�@�l�@�^5@���@���@�G�@���@��/@�Ĝ@��j@�r�@�A�@��@�\)@��y@��\@�=q@�X@�%@�z�@���@�ƨ@���@�l�@�~�@�/@��/@�Ĝ@�\)@���@��+@�=q@�@��-@�G�@���@�Ĝ@���@�z�@�bN@�9X@�1@��\@�hs@���@�;d@��@��@���@��;@��@���@�I�@��
@��y@�^5@�hs@�X@�`B@��/@��@��@��@�E�@�?}@�Q�@��
@�t�@�"�@�
=@�@��H@���@���@�ff@�ff@�n�@�~�@���@��\@�n�@�M�@�=q@�^5@��@�M�@��@�p�@��@�  @~�R@|�/@|(�@{��@{ƨ@{dZ@z~�@y�@w+@v�@v�y@w
=@w+@u�@u��@t�@t��@t��@t�@t�@s�
@s33@r��@rn�@q��@q��@qx�@qX@q�@p�u@p1'@pb@p �@o��@o;d@o�@o
=@n��@n@mO�@lj@k@i�@hbN@g�w@g\)@f�R@e?}@c@bn�@ahs@`b@_�@_K�@^�y@^�@^�y@_
=@_�@_�@_�@_+@_K�@_�;@_�@_�@_�@`  @`b@`A�@`�@ax�@a��@b^5@dI�@d�D@d�D@dz�@dz�@d(�@c��@b-@a��@a7L@`�`@`r�@_��@^ff@]�@]`B@]O�@]O�@Y�@Y��@Z�H@Zn�@Y�^@Y�@X��@X�u@XbN@XA�@X  @Wl�@W�@V�y@V��@W
=@W
=@V�@V�@V�y@Vff@U�@Up�@T��@T�D@T�D@Tj@TZ@T(�@S��@S�F@S33@S"�@S@R��@R�!@Rn�@R=q@R-@R�@Q�@QX@P��@PQ�@N��@Nv�@M�T@M?}@LI�@K�F@K�@K33@J��@I�@H��@Hr�@H  @G��@G\)@G+@G
=@F�y@F��@F{@E�@E�@D��@C�m@C��@CS�@C"�@B��@B~�@B~�@B~�@B�\@B�@B^5@BJ@A��@A�^@Ax�@Ax�@A�7@A��@A�@A�@A�@A�@A�@A��@Ax�@Ax�@AX@A7L@A�@A%@@��@@�9@@�u@@�u@@�u@@�@@�@@r�@@r�@@Q�@@bN@@bN@@r�@@�@@�u@@�u@@�@@r�@@bN@@  @@b@@b@@  @@A�@@Q�@@A�@@1'@@1'@@Ĝ@A&�@B�@D9X@D(�@D�@Dz�@D�/@E/@E`B@Ep�@E`B@EO�@E?}@D�/@DZ@D�@D�@D�@C�F@C�@C�@CdZ@CS�@CC�@C33@C@Bn�@B=q@BJ@A�^@A��@A��@A&�@@�9@@r�@@bN@@A�@@b@?��@?\)@?�@>�+@=`B@<��@<I�@<9X@<1@<�@<1@;��@;�
@;ƨ@;�F@;��@;��@;��@;�@;S�@;o@:�H@:��@:�!@:~�@9�@9%@8�@7�@6�y@6��@5�@5��@6@6��@7;d@6�y@6ȴ@6ȴ@6ȴ@7K�@8Q�@9x�@:-@9��@9��@9��@9��@9��@9�^@9��@9��@9x�@9X@9G�@9�@8��@8��@8��@8�@8r�@8r�@8bN@8 �@7�;@7�P@7;d@7�@7�@6��@6�y@6ȴ@6��@6��@6�+@6�+@6�+@6�+@6ff@6E�@6@6@5�@5�T@5�-@5�@5�@5�@5`B@5?}@5/@5V@5V@5V@5V@5V@4��@4�@4��@4�D@4z�@4j@4Z@4I�@4I�@4I�@49X@4(�@4(�@4�@3��@3�
@3ƨ@3��@3��@3t�@3"�@2�@2�@2�@2�H@2��@2�!@2�\@2�@1��@1�@1��@1��@1hs@1G�@1�@1%@1%@0�`@0�`@0�`@0�`@0�`@0�`@0��@0��@0��@0��@0��@0Ĝ@0�9@0�@0r�@0Q�@01'@0b@0  @0  @/�;@/�@/�P@/|�@/\)@/K�@/;d@/+@.��@.�y@.�@.ȴ@.ȴ@.�R@.�R@.�R@.��@.��@.��@.�+@.v�@.E�@.{@.@.@.@-�@-�@-�T@-��@-��@-��@-@-�-@-��@-�@-O�@-?}@-/@-V@,��@,�@,�/@,��@,��@,��@,��@,��@,�j@,�j@,�@,��@,z�@,j@,Z@,I�@,9X@,9X@,(�@,(�@,�@+��@+��@+�m@+�m@+�
@+�
@+ƨ@+�F@+�F@+��@+�@+�@+dZ@+dZ@+S�@+"�@+o@+o@+@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Ao7LAoG�AoK�AoO�AoS�AoS�AoXAo\)Ao`BAo\)Ao`BAodZAol�Aol�Ao|�Ao�7Ao��ApE�Ap�Aq��Aq��Aq��Aq��Aq�hAq�7Aq�Aq�PAq��Aq��Aq��Aq��Aq�PAq�7Aq�PAq�hAq��Aq��Aq��Aq�Aq�FAq�^Aq�^Aq�^Aq�^Aq�^Aq�^Aq�FAq�FAq�-Aq��Aq|�Ap�ApA�Al1Ag|�Ae%Ae;dAg&�Adn�Aa|�A`�+A_+A^ �A\z�AZ$�AWVAU��AS��AQ�AQ7LAP�APbNANI�AM�hAM;dAL��AL�yAL�9ALI�AL �AK��AJ��AJ�AJE�AI��AIdZAHn�AF��AE7LADv�ADAC��AB1AAXAAoA@��A@�DA@Q�A@�A?�#A?��A?�A?x�A?�A>r�A>Q�A>�+A>{A=�A;��A:bA933A8-A7��A6��A6$�A5�A5C�A4v�A4A�A3�mA3ƨA3�TA3p�A2Q�A2bA1ƨA1�7A1G�A0�`A0��A0�RA0jA0(�A/oA.$�A-��A-��A-
=A,Q�A+�;A+7LA*��A*(�A)K�A(�yA(�\A(5?A'�;A'�#A&�/A&�+A&��A&�HA&��A&A�A%�-A%��A%p�A%%A$��A#A#oA"�A"�HA"ȴA"��A#+A#;dA#7LA#+A#oA#��A$bNA$�DA$v�A$�+A$�+A$M�A$-A$�A$bA$A#�A#�-A"�A!��A!��A"VA"5?A!�;A!`BA!oA �9A ^5A (�A 1A��A��A�A��Az�A��Ax�AS�A�A1'A�;A;dA��A�uA1A��A�FAdZAȴAE�A1A�^A"�A�AffAG�A��A~�A�A�A��A�9An�AQ�AA�A$�A�wA�A�\AI�A�AS�A��AZA��A��A|�A�AffA��A�;A�hA�A
�yA
�/A
��A
bNA
JA	�#A	��A	7LAĜA�At�A33A&�AoA�#AdZAC�A�/A�!Az�AbNA1'A�PA �AC�A Ĝ@�t�@��@�7L@���@�K�@���@��T@��@�+@�5?@�?}@�!@�@�t�@���@�j@��@�@�Z@��@��@�E�@�J@�h@�?}@���@��@��@�n�@�X@ܣ�@���@��y@�ff@��@��@ڗ�@�ƨ@�b@���@�^5@�o@ҟ�@�E�@�1'@�;d@��@ə�@�j@�n�@��#@��
@�@��@�~�@\@��
@��/@�p�@��#@���@�X@Ĵ9@�o@��@��`@�9X@��@�|�@���@�/@�r�@�
=@�^5@�V@�5?@��^@��`@�r�@��@�J@�&�@��@�9X@��@��R@���@��#@���@�G�@��@��u@�(�@��
@�S�@���@�E�@�@��^@�V@���@�ƨ@�t�@���@�-@��@�@��@�G�@��@�1'@��F@���@�=q@�$�@�{@�J@�@��@�G�@�%@��@�I�@���@�C�@��@���@���@��\@�-@���@���@�(�@�l�@�^5@���@���@�G�@���@��/@�Ĝ@��j@�r�@�A�@��@�\)@��y@��\@�=q@�X@�%@�z�@���@�ƨ@���@�l�@�~�@�/@��/@�Ĝ@�\)@���@��+@�=q@�@��-@�G�@���@�Ĝ@���@�z�@�bN@�9X@�1@��\@�hs@���@�;d@��@��@���@��;@��@���@�I�@��
@��y@�^5@�hs@�X@�`B@��/@��@��@��@�E�@�?}@�Q�@��
@�t�@�"�@�
=@�@��H@���@���@�ff@�ff@�n�@�~�@���@��\@�n�@�M�@�=q@�^5@��@�M�@��@�p�@��@�  @~�R@|�/@|(�@{��@{ƨ@{dZ@z~�@y�@w+@v�@v�y@w
=@w+@u�@u��@t�@t��@t��@t�@t�@s�
@s33@r��@rn�@q��@q��@qx�@qX@q�@p�u@p1'@pb@p �@o��@o;d@o�@o
=@n��@n@mO�@lj@k@i�@hbN@g�w@g\)@f�R@e?}@c@bn�@ahs@`b@_�@_K�@^�y@^�@^�y@_
=@_�@_�@_�@_+@_K�@_�;@_�@_�@_�@`  @`b@`A�@`�@ax�@a��@b^5@dI�@d�D@d�D@dz�@dz�@d(�@c��@b-@a��@a7L@`�`@`r�@_��@^ff@]�@]`B@]O�@]O�@Y�@Y��@Z�H@Zn�@Y�^@Y�@X��@X�u@XbN@XA�@X  @Wl�@W�@V�y@V��@W
=@W
=@V�@V�@V�y@Vff@U�@Up�@T��@T�D@T�D@Tj@TZ@T(�@S��@S�F@S33@S"�@S@R��@R�!@Rn�@R=q@R-@R�@Q�@QX@P��@PQ�@N��@Nv�@M�T@M?}@LI�@K�F@K�@K33@J��@I�@H��@Hr�@H  @G��@G\)@G+@G
=@F�y@F��@F{@E�@E�@D��@C�m@C��@CS�@C"�@B��@B~�@B~�@B~�@B�\@B�@B^5@BJ@A��@A�^@Ax�@Ax�@A�7@A��@A�@A�@A�@A�@A�@A��@Ax�@Ax�@AX@A7L@A�@A%@@��@@�9@@�u@@�u@@�u@@�@@�@@r�@@r�@@Q�@@bN@@bN@@r�@@�@@�u@@�u@@�@@r�@@bN@@  @@b@@b@@  @@A�@@Q�@@A�@@1'@@1'@@Ĝ@A&�@B�@D9X@D(�@D�@Dz�@D�/@E/@E`B@Ep�@E`B@EO�@E?}@D�/@DZ@D�@D�@D�@C�F@C�@C�@CdZ@CS�@CC�@C33@C@Bn�@B=q@BJ@A�^@A��@A��@A&�@@�9@@r�@@bN@@A�@@b@?��@?\)@?�@>�+@=`B@<��@<I�@<9X@<1@<�@<1@;��@;�
@;ƨ@;�F@;��@;��@;��@;�@;S�@;o@:�H@:��@:�!@:~�@9�@9%@8�@7�@6�y@6��@5�@5��@6@6��@7;d@6�y@6ȴ@6ȴ@6ȴ@7K�@8Q�@9x�@:-@9��@9��@9��@9��@9��@9�^@9��@9��@9x�@9X@9G�@9�@8��@8��@8��@8�@8r�@8r�@8bN@8 �@7�;@7�P@7;d@7�@7�@6��@6�y@6ȴ@6��@6��@6�+@6�+@6�+@6�+@6ff@6E�@6@6@5�@5�T@5�-@5�@5�@5�@5`B@5?}@5/@5V@5V@5V@5V@5V@4��@4�@4��@4�D@4z�@4j@4Z@4I�@4I�@4I�@49X@4(�@4(�@4�@3��@3�
@3ƨ@3��@3��@3t�@3"�@2�@2�@2�@2�H@2��@2�!@2�\@2�@1��@1�@1��@1��@1hs@1G�@1�@1%@1%@0�`@0�`@0�`@0�`@0�`@0�`@0��@0��@0��@0��@0��@0Ĝ@0�9@0�@0r�@0Q�@01'@0b@0  @0  @/�;@/�@/�P@/|�@/\)@/K�@/;d@/+@.��@.�y@.�@.ȴ@.ȴ@.�R@.�R@.�R@.��@.��@.��@.�+@.v�@.E�@.{@.@.@.@-�@-�@-�T@-��@-��@-��@-@-�-@-��@-�@-O�@-?}@-/@-V@,��@,�@,�/@,��@,��@,��@,��@,��@,�j@,�j@,�@,��@,z�@,j@,Z@,I�@,9X@,9X@,(�@,(�@,�@+��@+��@+�m@+�m@+�
@+�
@+ƨ@+�F@+�F@+��@+�@+�@+dZ@+dZ@+S�@+"�@+o@+o@+@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BM�BM�BM�BM�BM�BO�BO�BO�BP�BO�BP�BQ�BT�BT�BYB_;Bn�B�B��B�^B�dB�dB�dB�dB�dB�jB�}BBĜBĜBŢBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB��B\)B<jBk�B��B�B�}B�LB�B��B��B�bB{�Bq�BcTBYBS�BP�BL�BD�B=qB;dB9XB8RB7LB49B2-B-B#�B�B�B�BuBDB��B�B�`B�HB�5B��B��B��B��BɺBǮBƨBŢBĜBÖBB�}B�dB�jB�}B�jB�FB��B�PB�%B{�Bv�Bq�Bk�BiyBdZB\)BZBXBW
B]/BYBN�BK�BG�BE�BB�B>wB<jB<jB8RB6FB,B#�B"�B�B�B�B�BVB1B1B
��B
��B
��B
��B
��B
��B
�B
�yB
�B
�B
��B
�B
�B
�B
�B
�sB
�`B
�;B
�B
�B
�B
�)B
�;B
�NB
�ZB
�ZB
�ZB
�`B
�BBBBB1B%BBBBBB  B
��B
�B
�BBBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�yB
�mB
�mB
�fB
�fB
�`B
�NB
�5B
�)B
�B
�B
��B
��B
��B
ǮB
ŢB
B
�wB
�qB
�RB
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�oB
�oB
�VB
�PB
�7B
�%B
�B
�B
�B
}�B
z�B
z�B
x�B
v�B
u�B
u�B
t�B
s�B
r�B
p�B
o�B
m�B
k�B
gmB
dZB
cTB
bNB
aHB
^5B
ZB
ZB
XB
XB
W
B
VB
T�B
R�B
J�B
E�B
C�B
>wB
:^B
7LB
49B
1'B
.B
.B
'�B
$�B
"�B
�B
�B
uB
VB
+B
B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�mB	�B	��B	��B	��B	ǮB	ĜB	ÖB	ÖB	ĜB	��B	�
B	�)B	�#B	�B	ȴB	�LB	��B	��B	��B	�oB	�bB	�VB	�1B	�B	� B	{�B	~�B	}�B	�B	�B	�hB	��B	��B	��B	��B	��B	��B	��B	�oB	�\B	�VB	�PB	�PB	�PB	�PB	�=B	�+B	�1B	�1B	�+B	�%B	�B	�B	�B	�B	� B	~�B	}�B	|�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�1B	�1B	�1B	�7B	�1B	�1B	�+B	�%B	�%B	�B	�B	�B	�B	�B	}�B	|�B	~�B	� B	� B	~�B	}�B	}�B	}�B	}�B	{�B	|�B	{�B	{�B	z�B	y�B	z�B	|�B	{�B	{�B	{�B	{�B	{�B	{�B	z�B	x�B	w�B	w�B	w�B	u�B	t�B	t�B	t�B	v�B	x�B	w�B	w�B	w�B	w�B	w�B	v�B	u�B	u�B	s�B	o�B	m�B	o�B	v�B	w�B	w�B	x�B	y�B	�B	�B	� B	~�B	~�B	~�B	�B	�B	~�B	}�B	|�B	{�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�+B	�+B	�1B	�7B	�=B	�PB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�-B	�-B	�-B	�-B	�'B	�'B	�'B	�3B	�3B	�-B	�-B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�'B	�-B	�XB	�jB	�qB	�wB	�}B	��B	B	ŢB	ɺB	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�)B	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
%B
+B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B

=B
DB
JB
JB
JB
PB
VB
\B
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
%�B
&�B
&�B
&�B
&�B
'�B
+B
+B
-B
0!B
49B
5?B
5?B
5?B
7LB
8RB
9XB
9XB
:^B
;dB
<jB
=qB
>wB
?}B
@�B
@�B
A�B
A�B
B�B
C�B
D�B
D�B
E�B
E�B
F�B
H�B
J�B
J�B
K�B
K�B
L�B
M�B
N�B
O�B
Q�B
R�B
T�B
W
B
YB
\)B
]/B
dZB
iyB
k�B
k�B
l�B
n�B
o�B
p�B
q�B
r�B
s�B
t�B
v�B
w�B
w�B
x�B
y�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
� B
� B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�1B
�7B
�=B
�DB
�PB
�JB
�PB
�VB
�\B
�bB
�bB
�bB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�uB
�oB
�hB
�hB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�-B
�3B
�3B
�9B
�9B
�9B
�?B
�FB
�FB
�LB
�RB
�RB
�XB
�^B
�^B
�^B
�^B
�dB
�qB
�wB
��B
��B
��B
B
B
ÖB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ƨB
ǮB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�#B
�#B
�)B
�5B
�;B
�;B
�;B
�BB
�BB
�HB
�HB
�TB
�ZB
�ZB
�`B
�fB
�mB
�sB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  B  B  BBBBBBBBBBBB%B%B%B%B+B+B+B+B+B+B1B1B	7B	7B	7B
=B
=B
=BDBDBDBJBJBJBJBPBPBPBVBVBVB\B\B\B\BbBhBhBhBhBh11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BM�BM�BM�BM�BM�BO�BO�BO�BP�BO�BP�BQ�BT�BT�BYB_;Bn�B�B��B�^B�dB�dB�dB�dB�dB�jB�}BBĜBĜBŢBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB��B\)B<jBk�B��B�B�}B�LB�B��B��B�bB{�Bq�BcTBYBS�BP�BL�BD�B=qB;dB9XB8RB7LB49B2-B-B#�B�B�B�BuBDB��B�B�`B�HB�5B��B��B��B��BɺBǮBƨBŢBĜBÖBB�}B�dB�jB�}B�jB�FB��B�PB�%B{�Bv�Bq�Bk�BiyBdZB\)BZBXBW
B]/BYBN�BK�BG�BE�BB�B>wB<jB<jB8RB6FB,B#�B"�B�B�B�B�BVB1B1B
��B
��B
��B
��B
��B
��B
�B
�yB
�B
�B
��B
�B
�B
�B
�B
�sB
�`B
�;B
�B
�B
�B
�)B
�;B
�NB
�ZB
�ZB
�ZB
�`B
�BBBBB1B%BBBBBB  B
��B
�B
�BBBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�yB
�mB
�mB
�fB
�fB
�`B
�NB
�5B
�)B
�B
�B
��B
��B
��B
ǮB
ŢB
B
�wB
�qB
�RB
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�uB
�oB
�oB
�VB
�PB
�7B
�%B
�B
�B
�B
}�B
z�B
z�B
x�B
v�B
u�B
u�B
t�B
s�B
r�B
p�B
o�B
m�B
k�B
gmB
dZB
cTB
bNB
aHB
^5B
ZB
ZB
XB
XB
W
B
VB
T�B
R�B
J�B
E�B
C�B
>wB
:^B
7LB
49B
1'B
.B
.B
'�B
$�B
"�B
�B
�B
uB
VB
+B
B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�mB	�B	��B	��B	��B	ǮB	ĜB	ÖB	ÖB	ĜB	��B	�
B	�)B	�#B	�B	ȴB	�LB	��B	��B	��B	�oB	�bB	�VB	�1B	�B	� B	{�B	~�B	}�B	�B	�B	�hB	��B	��B	��B	��B	��B	��B	��B	�oB	�\B	�VB	�PB	�PB	�PB	�PB	�=B	�+B	�1B	�1B	�+B	�%B	�B	�B	�B	�B	� B	~�B	}�B	|�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�1B	�1B	�1B	�7B	�1B	�1B	�+B	�%B	�%B	�B	�B	�B	�B	�B	}�B	|�B	~�B	� B	� B	~�B	}�B	}�B	}�B	}�B	{�B	|�B	{�B	{�B	z�B	y�B	z�B	|�B	{�B	{�B	{�B	{�B	{�B	{�B	z�B	x�B	w�B	w�B	w�B	u�B	t�B	t�B	t�B	v�B	x�B	w�B	w�B	w�B	w�B	w�B	v�B	u�B	u�B	s�B	o�B	m�B	o�B	v�B	w�B	w�B	x�B	y�B	�B	�B	� B	~�B	~�B	~�B	�B	�B	~�B	}�B	|�B	{�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�B	�+B	�+B	�1B	�7B	�=B	�PB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�-B	�-B	�-B	�-B	�'B	�'B	�'B	�3B	�3B	�-B	�-B	�!B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�'B	�'B	�-B	�XB	�jB	�qB	�wB	�}B	��B	B	ŢB	ɺB	��B	��B	��B	��B	�B	��B	��B	�B	�
B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�)B	�NB	�ZB	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
%B
+B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B

=B
DB
JB
JB
JB
PB
VB
\B
hB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
%�B
&�B
&�B
&�B
&�B
'�B
+B
+B
-B
0!B
49B
5?B
5?B
5?B
7LB
8RB
9XB
9XB
:^B
;dB
<jB
=qB
>wB
?}B
@�B
@�B
A�B
A�B
B�B
C�B
D�B
D�B
E�B
E�B
F�B
H�B
J�B
J�B
K�B
K�B
L�B
M�B
N�B
O�B
Q�B
R�B
T�B
W
B
YB
\)B
]/B
dZB
iyB
k�B
k�B
l�B
n�B
o�B
p�B
q�B
r�B
s�B
t�B
v�B
w�B
w�B
x�B
y�B
z�B
{�B
{�B
{�B
|�B
|�B
|�B
}�B
� B
� B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�1B
�7B
�=B
�DB
�PB
�JB
�PB
�VB
�\B
�bB
�bB
�bB
�hB
�oB
�oB
�oB
�oB
�oB
�uB
�{B
�{B
�{B
�{B
�{B
�{B
�{B
�uB
�oB
�hB
�hB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�-B
�3B
�3B
�9B
�9B
�9B
�?B
�FB
�FB
�LB
�RB
�RB
�XB
�^B
�^B
�^B
�^B
�dB
�qB
�wB
��B
��B
��B
B
B
ÖB
ĜB
ĜB
ŢB
ŢB
ŢB
ŢB
ƨB
ǮB
ɺB
ɺB
ɺB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�
B
�
B
�
B
�B
�B
�B
�B
�B
�#B
�#B
�)B
�5B
�;B
�;B
�;B
�BB
�BB
�HB
�HB
�TB
�ZB
�ZB
�`B
�fB
�mB
�sB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B  B  B  BBBBBBBBBBBB%B%B%B%B+B+B+B+B+B+B1B1B	7B	7B	7B
=B
=B
=BDBDBDBJBJBJBJBPBPBPBVBVBVB\B\B\B\BbBhBhBhBhBh11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20200524093743  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200524003745  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20200524003745  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200524003746  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200524003746  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20200524003746  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200524003747  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20200524003747  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200524003747  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200524003747                      G�O�G�O�G�O�                JA  ARUP                                                                        20200524005552                      G�O�G�O�G�O�                