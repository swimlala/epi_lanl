CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-23T03:37:01Z creation;2018-07-23T03:37:06Z conversion to V3.1;2019-12-19T07:33:58Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        |  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  `T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     |  ̼   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �L   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �P   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180723033701  20200116231518  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_259                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�p�/��1   @�p��[ @3���a@�dWU�=�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�ffA�ffA�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ Dټ�D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @xQ�@���@���Az�A<z�A\z�A|z�A�=qA���A���A�=qA�p�A�=qA�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B�B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B��\CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?�CAǮCCǮCEǮCGǮCIǮCK�HCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCsǮCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7��D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA��DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHq�DH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP��DQq�DQ��DRq�DR�DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[k�D[��D\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj��Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�u�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D��)D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�8�D�x�Dĸ�D���D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�x�DҸ�D���D�8�D�x�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dظ�D���D�8�D�x�Dٵ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�u�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D��)D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�+A�{A�{A�1A�  A���A��A��AԺ^A�=qA�5?A�l�A��9A��;A�\)A�A��jA�G�A��DA�r�A�z�A�{A�ƨA�~�A��A��wA�VA�jA���A�A���A��hA��A��A��PA�x�A�A�A��
A�A�A��yA���A�+A�+A�p�A�;dA��`A���A�
=A�-A���A�p�A�5?A��#A��uA�O�A��7A��FA��+A��A�r�A�  A�hsA�&�A���A�\)A�|�A�A�A��A�ffA�E�A�z�A��;A��DA��A��mA��PA��DA��A�1'A�l�A�7LA�l�A�ZA��`A�ZA�K�A�oA���A�$�A�x�A��A��`A7LA{��Az1'Ax�Aw�Av-As�;Aq��Ao|�AlbNAj��Ai��AiVAhE�Ag%AdJAat�A^�jAZ��AYG�AXVAWdZAV�DAT1AS�AP�/ANE�AM\)AKl�AI��AH5?AF�jAD�ABM�AAS�A@�9A@ZA@ �A?+A>�A<~�A;��A;;dA:�DA9dZA8ĜA8-A6A�A5��A3�FA1XA0$�A.^5A,�A,bA+?}A)hsA'�
A&��A&~�A%�-A$��A$n�A#�-A!�A r�A �\A �+A r�Ax�A�+A+A��A��A�
AffA�AG�A�A��A��Az�A��A��AA�FAz�Al�A"�A
��A	�PA^5A�;AC�A��AĜA�-A�A��A Q�@��F@�Ĝ@��@��H@�v�@��@��@���@��@�dZ@�+@���@�^5@��@��@�=q@�Z@�t�@@���@�j@��@��@��@�h@�hs@�7L@���@�r�@�@�@��`@��@���@��/@��y@���@܋D@ۥ�@�@ו�@���@���@��@�V@�33@�"�@�o@��@͉7@˝�@�\)@ɩ�@ǥ�@Ɵ�@�V@�J@őh@���@�Q�@�C�@�@�(�@�"�@�ȴ@�{@�x�@�%@���@���@��@���@�^5@��7@���@��@�C�@���@�5?@�p�@���@���@�bN@���@��\@�J@��@�%@�(�@�dZ@���@�~�@�$�@�J@���@��@��D@�(�@��P@�+@�@��H@�ȴ@���@�^5@�{@��#@�`B@���@��j@��u@��@�A�@�b@��;@���@��;@��w@��@�ƨ@���@���@�"�@��!@�v�@�hs@���@�{@�@�X@�O�@��7@���@�Q�@� �@�(�@��w@���@�t�@�\)@��@��R@�E�@�J@���@��-@���@�r�@���@�t�@��H@�V@��#@��-@��h@�G�@�&�@�V@��/@��@��u@�r�@�bN@�I�@��@���@�ƨ@�t�@��@�
=@���@��y@���@�n�@���@���@���@�x�@�O�@�V@��/@��9@�j@�Q�@�&�@���@� �@�b@��m@��;@�dZ@���@�1'@��w@�+@��@���@��@��y@��R@��+@�V@�E�@�-@���@���@�p�@�G�@�&�@���@���@��/@���@���@���@�I�@��@�b@���@��
@��w@���@���@��P@�l�@�C�@�33@���@���@��-@��7@�`B@�&�@��@�r�@�Z@��;@��P@��!@���@��^@���@��@�Ĝ@��@�r�@��
@�t�@��@��@�+@��H@�~�@��@���@��^@�?}@�%@��`@���@��u@�j@�I�@��P@���@�ƨ@��@�"�@��@�@��@���@�^5@�{@��@��@��7@�hs@�X@�/@��@��/@��@�I�@� �@�b@�  @��;@���@�K�@��@�
=@��H@�ȴ@���@���@���@��R@��!@�~�@�^5@��@��@�$�@���@�{@�=q@�ff@���@���@��R@���@��!@��\@��#@�G�@�&�@���@�Ĝ@��@�I�@�@�P@K�@+@~ȴ@~E�@}��@}@}`B@|(�@{�@{�F@{�F@{t�@{33@z�!@yhs@xĜ@xbN@w�@w;d@v�y@v�R@v��@v{@uO�@t��@t�j@t�j@t��@t�@tI�@s�F@sC�@r�@rJ@q��@p�`@p�@pb@o�@o�@o��@o��@o��@n��@m�T@m��@m�@l��@l1@kC�@j��@j�\@jM�@jJ@iG�@hr�@g�;@gl�@g;d@f�y@fV@e�T@e��@e�-@ep�@e�@d�j@d�D@dI�@cƨ@cS�@c@b=q@a��@a��@a�^@a��@ahs@a7L@`��@`�9@`��@`Q�@_�;@_|�@^��@^5?@]�@]�h@]`B@]V@\�j@\z�@\1@[�F@[��@["�@Z�H@Z��@Z��@Zn�@Y�@Yx�@X�9@XbN@XQ�@XA�@X �@W�@W�w@W|�@Wl�@W+@V�@V��@Vff@VV@V5?@V@U@U��@U�@U`B@UV@T�@TI�@S�m@S��@SS�@S"�@R��@R=q@RJ@Q�^@Q�7@Q7L@P�`@P�u@PA�@P �@O�@O;d@N��@N5?@N$�@M��@M��@M��@M/@LZ@K�m@KdZ@K"�@J��@J��@J~�@Jn�@Jn�@J�@I�^@I7L@H��@Hb@G�@G|�@G+@G�@G
=@F��@F{@E@E/@D��@D(�@C�m@Cƨ@Ct�@C@B^5@B�@A�@A��@Ax�@AG�@@�`@@��@@r�@@ �@?��@?\)@?�@>��@>5?@=��@=`B@=�@<�/@<Z@<�@;ƨ@;��@;dZ@:�@:�!@:~�@:^5@:M�@:M�@9��@97L@8bN@8A�@8b@7�w@7l�@7K�@7+@6��@5�@5@5p�@5?}@5�@4�@4�/@4z�@41@3�
@3�F@3S�@3C�@333@2�@2��@2��@2��@2~�@2^5@2�@1��@1hs@1%@0Ĝ@0�u@0bN@0  @/|�@.ȴ@.ff@.5?@-�@-�@-�-@-��@-�@-p�@-`B@-?}@,�@,��@,�@+�m@+ƨ@+�F@+��@+t�@+dZ@+33@+o@+o@*�@*��@*�!@*��@*n�@)��@)�^@)�7@)G�@)7L@)�@(��@(��@(Q�@(b@(  @'�@'|�@'\)@';d@'�@&��@&��@%�@%p�@%�@$�/@$�j@$��@$��@$�D@$z�@$j@$I�@$1@#ƨ@#��@#��@#dZ@#"�@"��@"^5@"-@"J@!��@!�7@!hs@!7L@!%@ �`@ ��@ ��@ ��@ �9@ bN@ 1'@ b@�;@�w@��@��@��@l�@;d@�y@�+@v�@ff@V@5?@5?@�@��@�-@p�@�@��@��@�@z�@I�@(�@��@dZ@C�@o@��@��@��@��@��@^5@-@J@��@��@�7@G�@�@�9@�@Q�@A�@b@�;@�;@�w@�P@K�@;d@+@�y@�R@ff@E�@5?@@�T@�-@�h@`B@/@�@�@�j@�D@Z@1@�
@�F@�F@��@t�@C�@@��@��@�\@n�@M�@=q@=q@=q@J@�@��@hs@G�@7L@&�@�@%@%@%@��@�u@bN@b@�;@�w@��@l�@\)@;d@+@+@
=@��@��@��@�y@�R@�+@ff@5?@��@��@�h@�@p�@?}@V@�@�j@��@z�@9X@1@�
@t�@S�@C�@C�@"�@
�@
�H@
��@
n�@
M�@
-@
J@	�@	�#@	��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�+A�{A�{A�1A�  A���A��A��AԺ^A�=qA�5?A�l�A��9A��;A�\)A�A��jA�G�A��DA�r�A�z�A�{A�ƨA�~�A��A��wA�VA�jA���A�A���A��hA��A��A��PA�x�A�A�A��
A�A�A��yA���A�+A�+A�p�A�;dA��`A���A�
=A�-A���A�p�A�5?A��#A��uA�O�A��7A��FA��+A��A�r�A�  A�hsA�&�A���A�\)A�|�A�A�A��A�ffA�E�A�z�A��;A��DA��A��mA��PA��DA��A�1'A�l�A�7LA�l�A�ZA��`A�ZA�K�A�oA���A�$�A�x�A��A��`A7LA{��Az1'Ax�Aw�Av-As�;Aq��Ao|�AlbNAj��Ai��AiVAhE�Ag%AdJAat�A^�jAZ��AYG�AXVAWdZAV�DAT1AS�AP�/ANE�AM\)AKl�AI��AH5?AF�jAD�ABM�AAS�A@�9A@ZA@ �A?+A>�A<~�A;��A;;dA:�DA9dZA8ĜA8-A6A�A5��A3�FA1XA0$�A.^5A,�A,bA+?}A)hsA'�
A&��A&~�A%�-A$��A$n�A#�-A!�A r�A �\A �+A r�Ax�A�+A+A��A��A�
AffA�AG�A�A��A��Az�A��A��AA�FAz�Al�A"�A
��A	�PA^5A�;AC�A��AĜA�-A�A��A Q�@��F@�Ĝ@��@��H@�v�@��@��@���@��@�dZ@�+@���@�^5@��@��@�=q@�Z@�t�@@���@�j@��@��@��@�h@�hs@�7L@���@�r�@�@�@��`@��@���@��/@��y@���@܋D@ۥ�@�@ו�@���@���@��@�V@�33@�"�@�o@��@͉7@˝�@�\)@ɩ�@ǥ�@Ɵ�@�V@�J@őh@���@�Q�@�C�@�@�(�@�"�@�ȴ@�{@�x�@�%@���@���@��@���@�^5@��7@���@��@�C�@���@�5?@�p�@���@���@�bN@���@��\@�J@��@�%@�(�@�dZ@���@�~�@�$�@�J@���@��@��D@�(�@��P@�+@�@��H@�ȴ@���@�^5@�{@��#@�`B@���@��j@��u@��@�A�@�b@��;@���@��;@��w@��@�ƨ@���@���@�"�@��!@�v�@�hs@���@�{@�@�X@�O�@��7@���@�Q�@� �@�(�@��w@���@�t�@�\)@��@��R@�E�@�J@���@��-@���@�r�@���@�t�@��H@�V@��#@��-@��h@�G�@�&�@�V@��/@��@��u@�r�@�bN@�I�@��@���@�ƨ@�t�@��@�
=@���@��y@���@�n�@���@���@���@�x�@�O�@�V@��/@��9@�j@�Q�@�&�@���@� �@�b@��m@��;@�dZ@���@�1'@��w@�+@��@���@��@��y@��R@��+@�V@�E�@�-@���@���@�p�@�G�@�&�@���@���@��/@���@���@���@�I�@��@�b@���@��
@��w@���@���@��P@�l�@�C�@�33@���@���@��-@��7@�`B@�&�@��@�r�@�Z@��;@��P@��!@���@��^@���@��@�Ĝ@��@�r�@��
@�t�@��@��@�+@��H@�~�@��@���@��^@�?}@�%@��`@���@��u@�j@�I�@��P@���@�ƨ@��@�"�@��@�@��@���@�^5@�{@��@��@��7@�hs@�X@�/@��@��/@��@�I�@� �@�b@�  @��;@���@�K�@��@�
=@��H@�ȴ@���@���@���@��R@��!@�~�@�^5@��@��@�$�@���@�{@�=q@�ff@���@���@��R@���@��!@��\@��#@�G�@�&�@���@�Ĝ@��@�I�@�@�P@K�@+@~ȴ@~E�@}��@}@}`B@|(�@{�@{�F@{�F@{t�@{33@z�!@yhs@xĜ@xbN@w�@w;d@v�y@v�R@v��@v{@uO�@t��@t�j@t�j@t��@t�@tI�@s�F@sC�@r�@rJ@q��@p�`@p�@pb@o�@o�@o��@o��@o��@n��@m�T@m��@m�@l��@l1@kC�@j��@j�\@jM�@jJ@iG�@hr�@g�;@gl�@g;d@f�y@fV@e�T@e��@e�-@ep�@e�@d�j@d�D@dI�@cƨ@cS�@c@b=q@a��@a��@a�^@a��@ahs@a7L@`��@`�9@`��@`Q�@_�;@_|�@^��@^5?@]�@]�h@]`B@]V@\�j@\z�@\1@[�F@[��@["�@Z�H@Z��@Z��@Zn�@Y�@Yx�@X�9@XbN@XQ�@XA�@X �@W�@W�w@W|�@Wl�@W+@V�@V��@Vff@VV@V5?@V@U@U��@U�@U`B@UV@T�@TI�@S�m@S��@SS�@S"�@R��@R=q@RJ@Q�^@Q�7@Q7L@P�`@P�u@PA�@P �@O�@O;d@N��@N5?@N$�@M��@M��@M��@M/@LZ@K�m@KdZ@K"�@J��@J��@J~�@Jn�@Jn�@J�@I�^@I7L@H��@Hb@G�@G|�@G+@G�@G
=@F��@F{@E@E/@D��@D(�@C�m@Cƨ@Ct�@C@B^5@B�@A�@A��@Ax�@AG�@@�`@@��@@r�@@ �@?��@?\)@?�@>��@>5?@=��@=`B@=�@<�/@<Z@<�@;ƨ@;��@;dZ@:�@:�!@:~�@:^5@:M�@:M�@9��@97L@8bN@8A�@8b@7�w@7l�@7K�@7+@6��@5�@5@5p�@5?}@5�@4�@4�/@4z�@41@3�
@3�F@3S�@3C�@333@2�@2��@2��@2��@2~�@2^5@2�@1��@1hs@1%@0Ĝ@0�u@0bN@0  @/|�@.ȴ@.ff@.5?@-�@-�@-�-@-��@-�@-p�@-`B@-?}@,�@,��@,�@+�m@+ƨ@+�F@+��@+t�@+dZ@+33@+o@+o@*�@*��@*�!@*��@*n�@)��@)�^@)�7@)G�@)7L@)�@(��@(��@(Q�@(b@(  @'�@'|�@'\)@';d@'�@&��@&��@%�@%p�@%�@$�/@$�j@$��@$��@$�D@$z�@$j@$I�@$1@#ƨ@#��@#��@#dZ@#"�@"��@"^5@"-@"J@!��@!�7@!hs@!7L@!%@ �`@ ��@ ��@ ��@ �9@ bN@ 1'@ b@�;@�w@��@��@��@l�@;d@�y@�+@v�@ff@V@5?@5?@�@��@�-@p�@�@��@��@�@z�@I�@(�@��@dZ@C�@o@��@��@��@��@��@^5@-@J@��@��@�7@G�@�@�9@�@Q�@A�@b@�;@�;@�w@�P@K�@;d@+@�y@�R@ff@E�@5?@@�T@�-@�h@`B@/@�@�@�j@�D@Z@1@�
@�F@�F@��@t�@C�@@��@��@�\@n�@M�@=q@=q@=q@J@�@��@hs@G�@7L@&�@�@%@%@%@��@�u@bN@b@�;@�w@��@l�@\)@;d@+@+@
=@��@��@��@�y@�R@�+@ff@5?@��@��@�h@�@p�@?}@V@�@�j@��@z�@9X@1@�
@t�@S�@C�@C�@"�@
�@
�H@
��@
n�@
M�@
-@
J@	�@	�#@	��@	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+B.B.B.B/B.B.B+BoBN�BŢB�9B�sBVB2-B-B6FBK�BVB`BB}�B�B�%B�B�7B�1B�B��B��B��B��B��B��B�B�B��B��B��B��B��B�oB�B�B�oB�JB�+B� Bt�B� B�B�B}�Bz�By�BiyBhsBy�B~�Bz�Bn�BiyBhsB`BBcTBVBR�BP�B=qB2-B�B��B  B�fB�'B�=B}�B?}B�BuB
�B%BVB&�B�BB
�B
��B
�B
��B
x�B
J�B
H�B
,B
uB
-B
'�B
JB

=B	�sB	��B	�}B	��B	��B	��B	��B	�B	l�B	D�B	(�B	$�B��B	(�B	0!B	'�B	�B��B	PB�B�B�B�B�B��BƨB�XB�jB��B��B��B��B�wB�-B�B�'B�?B�B��B��B��B�JB��B�By�B�+B�%B�B�=B�Bk�Bl�Br�Bv�Bn�BffBl�BffBH�BdZBx�Br�BffBK�B$�B7LB9XBB�B=qB9XBJ�BJ�BE�B+B=qB9XBE�BH�BF�BL�B=qB@�BM�BL�B<jB:^BH�BD�B8RB:^B?}BA�BF�B7LBD�B<jBH�BR�B[#B\)B]/B[#BaHBjBjBe`Be`B^5BO�BT�B[#BjBjBk�BiyBjBq�B}�B�B� B~�B|�B|�Bx�Bl�Bz�Bw�Bq�BjBgmBs�Bq�Bq�Bk�BjBt�B|�Bu�B{�B|�B�{B�hB�DB� Bz�B�VB�B�B�oB��B��B��B��B��B��B�{B��B��B�3B�-B�?B�RB�RB�FB�}BÖBĜB��BŢBÖB��B��B��B�B�B�NB�HB�5B�/B�sB�B��B�B��B��B	B	B	B	  B	  B	B	B	+B		7B	PB	\B	bB	bB	bB	hB	oB	{B	�B	�B	�B	�B	�B	�B	"�B	&�B	+B	/B	33B	2-B	7LB	;dB	;dB	@�B	C�B	B�B	F�B	XB	T�B	T�B	YB	[#B	XB	[#B	`BB	ffB	e`B	k�B	n�B	n�B	m�B	m�B	n�B	r�B	r�B	r�B	m�B	s�B	s�B	r�B	t�B	w�B	y�B	}�B	� B	~�B	�B	�B	�%B	�+B	�=B	�DB	�JB	�PB	�VB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�3B	�^B	�wB	�^B	�XB	�qB	�qB	�qB	�wB	ĜB	ŢB	ǮB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	��B	��B	�B	�/B	�;B	�/B	�#B	�BB	�HB	�5B	�)B	�#B	�
B	�;B	�HB	�)B	�;B	�;B	�HB	�;B	�BB	�`B	�`B	�TB	�TB	�TB	�TB	�`B	�mB	�ZB	�sB	�B	�yB	�yB	�yB	�yB	�fB	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
  B
  B
B
B
B
B
B
+B
DB
JB
\B
\B
\B
\B
\B
PB
	7B

=B
\B
\B
\B
\B
VB
\B
hB
hB
oB
oB
hB
bB
uB
oB
bB
oB
�B
�B
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
"�B
!�B
�B
�B
"�B
!�B
 �B
 �B
"�B
"�B
$�B
$�B
#�B
"�B
 �B
"�B
$�B
%�B
$�B
$�B
%�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
'�B
'�B
'�B
'�B
)�B
,B
-B
-B
,B
,B
-B
-B
.B
.B
.B
.B
.B
.B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
33B
33B
2-B
2-B
33B
5?B
7LB
7LB
6FB
6FB
7LB
6FB
7LB
6FB
6FB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
8RB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
:^B
8RB
9XB
;dB
=qB
<jB
<jB
=qB
;dB
:^B
=qB
>wB
?}B
?}B
@�B
@�B
A�B
@�B
?}B
>wB
>wB
>wB
>wB
@�B
A�B
A�B
A�B
A�B
@�B
?}B
@�B
@�B
A�B
@�B
C�B
D�B
C�B
C�B
B�B
E�B
F�B
F�B
G�B
G�B
F�B
G�B
H�B
G�B
G�B
G�B
H�B
H�B
F�B
G�B
J�B
J�B
K�B
J�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
M�B
K�B
K�B
K�B
O�B
O�B
N�B
N�B
O�B
O�B
M�B
N�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
S�B
R�B
S�B
T�B
S�B
S�B
T�B
T�B
T�B
S�B
R�B
R�B
T�B
S�B
T�B
T�B
T�B
S�B
S�B
S�B
VB
XB
XB
YB
YB
ZB
ZB
ZB
ZB
ZB
YB
YB
XB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
[#B
ZB
[#B
]/B
\)B
]/B
]/B
]/B
\)B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
]/B
]/B
^5B
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
aHB
bNB
cTB
cTB
bNB
bNB
aHB
cTB
cTB
dZB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
dZB
cTB
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
e`B
e`B
e`B
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
gmB
iyB
iyB
iyB
iyB
k�B
k�B
k�B
jB
iyB
jB
jB
k�B
jB
jB
jB
jB
jB
l�B
l�B
m�B
l�B
l�B
m�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
w�B
y�B
z�B
z�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
{�B
{�B
|�B
|�B
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+QB.B.IB./B/iB.�B/�B0�BOBb�B�B�*B�"BNB3�B/�B9>BM�BX_BbhB~�B��B��B�%B�=B�lB��B�YB�HB�LB�FB��B��B�]B�wB��B��B�	B��B��B��B�SB��B�B�6B�B�oBv�B��B��B��B~�B{�Bz�BkkBjBzDB.B{dBo�Bj�BiDBa�Bd&BW�BT,BRB?}B3�B vB��B'B�B�B�(B��BD�B#�B�B
�aB�B.B'B$B�B
��B
�@B
��B
�vB
}<B
O�B
L�B
1AB
YB
.�B
)yB
�B
�B	�kB	��B	�AB	�QB	��B	��B	�YB	��B	n�B	HfB	,�B	(XB	�B	*�B	1vB	)_B	/B	 �B	�B�/B�qB��BۦB�+B�PB�B��B��B� B��BуBΥB� B��B�!B�GB�B�IB��B��B�4B��B��B��B|�B�B��B�GB�^B��Bn/Bn�Bs�Bw�Bo�Bg�BmCBg�BKxBeBx�Br�Bg8BM�B(�B9$B;JBDB?B;dBK�BK�BGB-�B>�B;BF�BI�BHBM�B?HBA�BNVBM�B>wB;�BI�BE�B:^B<6BA BC-BG�B9rBE�B>]BI�BS�B[�B\�B]�B\)Ba�Bj�Bj�BfBe�B_BQ�BVSB\�Bk6BkQBlqBj�Bk�Br|B~BB�UB�OBcB}qB}�By�Bn/B{Bx�Br�BlBh�Bt�Br�Br�Bm)Bl"BvB}�Bw�B}VB~B��B��B��B�UB|PB��B��B��B�&B�B�-B�VB�]B�OB��B��B��B��B��B��B��B��B��B�2B��B��B�B�UB�?BāB�dBуB�{BּBںB��B�B��B�5B�B�6B��B�B�tB�^B	UB	�B	aB	 �B	 �B	�B	�B	�B		�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	 B	# B	'B	+B	/OB	33B	2|B	7�B	;�B	<B	@�B	C�B	CGB	F�B	WsB	UgB	UgB	YKB	[=B	X�B	[�B	`�B	f�B	e�B	k�B	n�B	n�B	m�B	m�B	o B	r�B	r�B	r�B	ncB	t9B	t9B	sMB	u?B	xRB	zDB	~BB	�4B	cB	�GB	�SB	�tB	�zB	�rB	�xB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�-B	� B	�&B	�,B	�B	��B	��B	��B	�;B	�UB	�oB	��B	�DB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�#B	��B	�B	�B	�B	�(B	� B	�B	�B	� B	�:B	�:B	�MB	�+B	�EB	�KB	�KB	�QB	�kB	�QB	�eB	�_B	�sB	՛B	ԕB	چB	�~B	�pB	�~B	��B	�vB	�|B	޸B	ܬB	��B	��B	�pB	�|B	��B	ߊB	ߤB	�|B	߾B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�%B	�9B	�B	�B	�*B	�B	�B	�*B	�0B	�"B	�.B	�.B	�.B
 B
 B
;B
 4B
 4B
 OB
;B
UB
-B
9B
SB
EB
DB
JB
\B
�B
�B
�B
�B
�B
	�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	B
�B
�B
�B
�B
B
B
�B
�B
B
B
B
�B
B
 �B
"�B
#�B
#B
"B
VB
 'B
#B
"B
!-B
!-B
# B
# B
%B
%B
$&B
#TB
!-B
# B
%,B
&B
%,B
%,B
&2B
)B
)*B
)*B
)*B
)*B
)*B
)*B
(>B
(XB
(>B
(XB
*0B
,"B
-)B
-CB
,WB
,=B
-CB
-CB
.IB
.}B
.cB
.cB
.cB
.}B
0UB
1[B
1[B
1[B
1[B
1[B
2|B
2aB
3hB
3�B
3hB
4TB
3hB
3hB
2|B
2|B
3�B
5�B
7fB
7fB
6�B
6zB
7�B
6zB
7fB
6zB
6�B
7�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
7�B
7�B
7�B
8�B
9�B
9�B
9�B
9�B
8�B
:�B
:�B
:�B
:�B
:�B
;�B
;�B
;�B
:�B
8�B
9�B
;�B
=�B
<�B
<�B
=�B
;�B
:�B
=�B
>�B
?�B
?�B
@�B
@�B
A�B
@�B
?�B
>�B
>�B
>�B
>�B
@�B
A�B
A�B
A�B
A�B
@�B
?�B
@�B
@�B
A�B
@�B
C�B
D�B
C�B
C�B
B�B
E�B
F�B
F�B
G�B
G�B
F�B
G�B
H�B
G�B
G�B
G�B
H�B
H�B
GB
HB
J�B
J�B
LB
K)B
K�B
K�B
MB
MB
MB
NB
N"B
OB
N�B
NB
LB
LB
L0B
O�B
P.B
OB
OB
P.B
PB
N<B
O(B
Q4B
Q4B
R B
R B
R B
R B
QNB
RTB
S&B
T,B
S@B
T,B
UB
T,B
T,B
UB
UMB
U2B
T,B
S&B
S[B
U2B
TFB
UMB
U2B
U2B
TFB
TFB
TaB
V9B
XEB
XEB
YKB
YKB
Z7B
ZQB
ZQB
ZQB
ZQB
YKB
YKB
X_B
ZQB
[WB
[=B
[WB
[WB
[WB
[WB
\]B
\CB
\xB
\]B
\]B
\]B
[WB
ZkB
[WB
]dB
\]B
]IB
]dB
]dB
\xB
]dB
^OB
^jB
^jB
^jB
_pB
_pB
_�B
_pB
]�B
]~B
^�B
`vB
a|B
b�B
b�B
bhB
bhB
b�B
b�B
b�B
a|B
b�B
c�B
c�B
b�B
b�B
a�B
c�B
c�B
d�B
c�B
d�B
d�B
d�B
d�B
e`B
ezB
ezB
ezB
d�B
c�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
e�B
e�B
e�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
g�B
i�B
i�B
i�B
i�B
k�B
k�B
k�B
j�B
i�B
j�B
j�B
k�B
j�B
j�B
j�B
j�B
j�B
l�B
l�B
m�B
l�B
l�B
m�B
m�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
q�B
q�B
q�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
r�B
r�B
r�B
r�B
r�B
s�B
t�B
t�B
t�B
t�B
t�B
s�B
s�B
r�B
s�B
s�B
s�B
t�B
t�B
vB
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
w�B
w�B
xB
xB
xB
xB
xB
xB
xB
xB
xB
y	B
xB
zB
{B
z�B
zB
y�B
zB
zB
zB
{B
{B
|B
|B
|B
}B
}B
}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<lR<�� <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.22(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807140037512018071400375120180714003751201807140200252018071402002520180714020025201807150030192018071500301920180715003019  JA  ARFMdecpA19c                                                                20180723123519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723033701  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723033704  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723033704  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723033705  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723033705  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723033705  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723033705  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20180723033706                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723040122                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180710153422  CV  JULD            G�O�G�O�FÅ�                JM  ARCAJMQC2.0                                                                 20180713153751  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180713153751  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180713170025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180714153019  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231518                      G�O�G�O�G�O�                