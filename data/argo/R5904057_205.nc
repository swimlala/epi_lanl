CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-10-08T22:00:30Z creation      
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
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  `(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ̐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191008220030  20191008220030  5904057 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5006                            2B  A   NAVIS_A                         0305                            082713                          863 @��d��1   @���Q��@6xQ���e!?|�h1   GPS     Primary sampling: mixed [deep: discrete, shallow: continuous]                                                                                                                                                                                                      �A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct�Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D���D�<�DĀ D�� D�3D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ D҃3D�� D�  D�@ D�|�D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ Dؼ�D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D�3D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @%�@q�@���@���Az�A<z�A\z�A|z�A�=qA�=qA�=qA�=qA�=qA�
>A�=qA�=qB�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bn�RBw�B�B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\B��\BÏ\BǏ\Bˏ\BϏ\Bӏ\B׏\Bۏ\Bߏ\B�\B�\B�\B�\B�\B��\B��\B��\CǮCǮCǮCǮC	ǮCǮCǮCǮCǮCǮCǮCǮCǮCǮCǮC�C!ǮC#ǮC%ǮC'ǮC)ǮC+ǮC-ǮC/ǮC1ǮC3ǮC5ǮC7ǮC9ǮC;ǮC=ǮC?ǮCAǮCCǮCEǮCGǮCIǮCKǮCMǮCOǮCQǮCSǮCUǮCWǮCYǮC[ǮC]ǮC_ǮCaǮCcǮCeǮCgǮCiǮCkǮCmǮCoǮCqǮCs�HCuǮCwǮCyǮC{ǮC}ǮCǮC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��
C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D q�D ��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��D	q�D	��D
q�D
��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dk�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��Dq�D��DxRD�RDq�D��Dq�D��Dq�D��Dq�D��Dq�D��D q�D ��D!q�D!��D"q�D"��D#q�D#��D$q�D$��D%q�D%��D&q�D&��D'q�D'��D(q�D(��D)q�D)��D*q�D*��D+q�D+��D,q�D,��D-q�D-��D.q�D.��D/q�D/��D0q�D0��D1q�D1��D2q�D2��D3q�D3��D4q�D4��D5q�D5��D6q�D6��D7q�D7�D8q�D8��D9q�D9��D:q�D:��D;q�D;��D<q�D<��D=q�D=��D>q�D>��D?q�D?��D@q�D@��DAq�DA�DBq�DB��DCq�DC��DDq�DD��DEq�DE��DFq�DF��DGq�DG��DHxRDH��DIq�DI��DJq�DJ��DKq�DK��DLq�DL��DMq�DM��DNq�DN��DOq�DO��DPq�DP�RDQq�DQ��DRq�DR��DSq�DS��DTq�DT��DUq�DU��DVq�DV��DWq�DW��DXq�DX��DYq�DY��DZq�DZ��D[q�D[�RD\q�D\��D]q�D]��D^q�D^��D_q�D_��D`q�D`��Daq�Da��Dbq�Db��Dcq�Dc��Ddq�Dd��Deq�De��Dfq�Df��Dgq�Dg��Dhq�Dh��Diq�Di��Djq�Dj�Dkq�Dk��Dlq�Dl��Dmq�Dm��Dnq�Dn��Doq�Do��Dpq�Dp��Dqq�Dq��Drq�Dr��Dsq�Ds��Dtq�Dt��Duq�Du��Dvq�Dv��Dwq�Dw��Dxq�Dx��Dyq�Dy��Dzq�Dz��D{q�D{��D|q�D|��D}q�D}��D~q�D~��Dq�D��D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�5�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D��)D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D���D���D�8�D�x�D¸�D���D�8�D�x�Dø�D���D�5�D�x�Dĸ�D��)D�8�D�x�DŸ�D���D�8�D�x�DƸ�D���D�8�D�x�DǸ�D���D�8�D�x�Dȸ�D���D�8�D�x�Dɸ�D���D�8�D�x�Dʸ�D���D�8�D�x�D˸�D���D�8�D�x�D̸�D���D�8�D�x�D͸�D���D�8�D�x�Dθ�D���D�8�D�x�Dϸ�D���D�8�D�x�Dи�D���D�8�D�x�DѸ�D���D�8�D�|)DҸ�D���D�8�D�u�DӸ�D���D�8�D�x�DԸ�D���D�8�D�x�Dո�D���D�8�D�x�Dָ�D���D�8�D�x�D׸�D���D�8�D�x�Dص�D���D�8�D�x�Dٸ�D���D�8�D�x�Dڸ�D���D�8�D�x�D۸�D���D�8�D�x�Dܸ�D���D�8�D�x�Dݸ�D���D�8�D�x�D޸�D���D�8�D�x�D߸�D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D���D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�8�D�x�D��D���D�<)D�|)D���D���D�5�D�x�D���D���D�8�D�x�D���D���D�8�D�|)D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A�^A�jA�wA���A�A�A���A���A���A���A�A�A�A�A�ȴA�jA���A���A�ƨA� �A��AٸRA�+AͰ!A�M�A��A�VA�p�A�ZA��FA��A���A��HA�jA��A�+A�O�A��`A��RA���A��uA� �A��/A�ĜA�5?A�hsA��FA���A���A�JA��;A�K�A�Q�A�Q�A���A��PA�A�A�ffA��A��9A�n�A��;A���A���A�r�A���A�{A��A��PA��A��hA�`BA�A�K�A���A���A��A�O�A���A��FA�ffA�x�A��RA�S�A�"�A��jA���A��hA��A�K�A��`A���A�`BA��RA�x�A��jA�M�AhsA|  Ay��AvĜAtI�Aq��Ao��Ao%An$�Ak?}Ad��A_��A^�A]x�A\��A\��A\��A\�+AY�TAVn�ASp�AQ`BAO��ANZAM��AMC�AL��AK�FAKAI�
AHz�AF�`AF1AE|�AE
=ADȴADA�AC��ABĜAA?}A?�wA?A>��A>-A<ZA9"�A6�A4ĜA2VA1��A1`BA0�/A-t�A)�;A'�;A&ZA%K�A#�-A"^5A jAA��At�A��A�HA��A`BA+A�/AE�A�AC�A�A��A`BA�A�AdZAz�AbA��A�A�yA��AjA��A��A�A�9A(�A"�AffA+A
�jA
��A
^5A	x�A�RAA�A�wA/AVA��A�A��Al�A�\A|�A �@��F@�@��@�ff@���@��@���@�@� �@�R@�%@�F@��@�?}@�o@�5?@���@�j@�K�@�@�@�1@�+@���@�1@�;d@���@�@� �@۾w@�=q@ٺ^@��@�bN@��@֟�@�J@��@��@�@�J@ղ-@�r�@ӍP@�+@��@�ȴ@ҏ\@�`B@�n�@�Ĝ@�1'@˾w@��@�~�@�5?@�J@ə�@�&�@ȼj@�bN@Ǯ@�-@���@�-@�$�@Ĵ9@§�@��@���@���@���@�1'@�b@��F@�\)@��R@�n�@��@���@�j@���@��@�5?@��-@��7@���@��P@�@���@�^5@��@���@��@�Q�@� �@�I�@�Q�@�z�@�V@�ƨ@�n�@�dZ@�j@���@� �@�"�@��@��@�b@�o@���@�n�@���@�G�@���@��@��m@��;@��w@�ƨ@��;@��@���@�{@�O�@�bN@�|�@�"�@�33@�
=@��H@�ȴ@���@�-@�{@���@�z�@��F@�\)@�+@��@���@�M�@�O�@���@���@�1'@�1'@�j@���@�hs@��7@��j@��P@�;d@��@��R@�E�@�@�@�x�@�`B@��@��/@�j@�  @��@��@�@��\@�$�@��@���@��-@�7L@�%@��j@��@�Z@�A�@� �@��@��@���@�l�@�^5@��#@��-@�x�@���@�bN@�  @�  @��;@�l�@�l�@��@�
=@��y@�v�@�ff@���@�V@�O�@�%@�%@��`@��j@���@��@�bN@�I�@�9X@�1'@�b@��@��w@���@��@�t�@�\)@�\)@�S�@�;d@�@���@���@�@�o@�+@�"�@��H@��R@���@���@���@��\@�V@��T@��h@�x�@�G�@��@�9X@�t�@�\)@�\)@�K�@�;d@�@��H@�ȴ@���@�ff@�^5@��@��^@�x�@�G�@�&�@��9@�@�w@l�@
=@}�@}�@}�h@}O�@}O�@}?}@}/@}�@|��@|�@|��@|��@{��@{�@{S�@{33@z�H@z�!@zM�@zJ@y��@yG�@x��@xbN@w�@w�@w�P@w\)@w
=@v��@v�@v��@vff@vE�@u�-@u`B@t�@tI�@s�
@sdZ@r�@r~�@r��@r=q@q�#@qX@p�9@o�;@o|�@o;d@n�@n��@nff@m�@m�@m?}@l�@l(�@k�m@k�F@kC�@j�@j��@j~�@j~�@j��@k@k@j��@j~�@i��@i�7@ihs@h�9@g��@f�R@f�+@fV@f$�@e��@d��@d�@d(�@c�
@cdZ@co@b�H@b�H@b��@bn�@a��@a��@a�7@aG�@`�9@_�@_�@_��@_��@_|�@_\)@_+@^�@^ff@]@]`B@\9X@[ƨ@[�F@[��@[�@[C�@["�@Z�@Z��@Zn�@Z-@Y��@YX@Y&�@X��@XĜ@XA�@X  @W�;@W�w@W�P@W+@V�+@U��@U/@T�/@T�j@T�@TI�@S�F@SC�@R�@R��@R�\@R�\@R��@RM�@R�@Q��@PĜ@Pr�@O�@O�P@Ol�@O\)@O+@N��@Nv�@N@M�-@MO�@M�@L�@L��@L��@L��@LZ@LI�@L(�@K�
@K��@KS�@K"�@J�H@J=q@I�#@I�7@I7L@HĜ@Hr�@Hb@G�@Gl�@G;d@F��@Fv�@F$�@F{@E�h@EO�@D�@Dj@C�
@C��@C��@C�@Ct�@Co@B��@B��@Bn�@Bn�@B^5@A�7@A�@@��@@b@?�@?|�@?K�@?;d@?+@?�@>�@>�R@>��@>v�@=�@=`B@=�@<��@<�D@<I�@<(�@;��@;ƨ@;t�@;S�@;33@;o@;@:��@:-@9x�@8��@8�9@8��@8�u@8�u@8�@8Q�@8 �@8  @7��@7;d@6��@6�+@6$�@5�@5�T@5@5p�@4�/@4�D@4z�@4Z@49X@3��@3�F@3t�@3dZ@3S�@333@3o@2��@2^5@1��@1�@1��@1hs@1�@0��@0��@0  @/�w@/�P@/l�@/l�@/l�@/;d@/;d@/+@/�@/�@.�R@.5?@.$�@.$�@-��@-?}@-V@,�j@,9X@+�
@*�@)�#@)��@)X@)7L@)&�@)�@(�`@(Ĝ@(�9@(�9@(�u@(Q�@'�@'l�@'�@&�@&�R@&�R@&��@&��@&��@&v�@&V@&@%@%�-@%�h@%�@%p�@%O�@%/@%�@$�/@$�D@$9X@#��@#"�@"��@"��@"n�@"=q@!��@!7L@!%@ ��@ ��@ �u@ �u@ bN@�@��@\)@+@ȴ@�+@V@@@/@�D@Z@9X@�@�m@t�@C�@33@"�@�@�!@��@~�@n�@=q@J@�#@��@��@x�@x�@G�@�@�`@�u@1'@ �@ �@b@  @�@��@�@|�@;d@+@�@v�@E�@5?@�@�T@��@�@`B@�@�@��@Z@1@�m@�
@��@dZ@��@~�@��@X@X@X@X@X@X@X@7L@%@��@�`@�`@Ĝ@�9@�@r�@b@  @�@|�@K�@��@�@ȴ@E�@�T@@p�@�@�j@z�@z�@Z@�m@�F@��@��@�@�@t�@dZ@S�@33@
�@
�\@
^5@
M�@
=q@
-@	��@	��@	��@	��@	��@	�7@	�7@	��@	�7@	�7@	x�@	G�@	7L@	&�@	�@	%@��@Ĝ@Ĝ@�9@�u@�@bN@1'@ �@b@b@�;@�w@��@l�@+@�@�@�@�@�+@E�@E�@5?@{@�@@��@�@`B@?}@V@��@��@�@�@��@��@j@Z@I�@9X@�m@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A�^A�jA�wA���A�A�A���A���A���A���A�A�A�A�A�ȴA�jA���A���A�ƨA� �A��AٸRA�+AͰ!A�M�A��A�VA�p�A�ZA��FA��A���A��HA�jA��A�+A�O�A��`A��RA���A��uA� �A��/A�ĜA�5?A�hsA��FA���A���A�JA��;A�K�A�Q�A�Q�A���A��PA�A�A�ffA��A��9A�n�A��;A���A���A�r�A���A�{A��A��PA��A��hA�`BA�A�K�A���A���A��A�O�A���A��FA�ffA�x�A��RA�S�A�"�A��jA���A��hA��A�K�A��`A���A�`BA��RA�x�A��jA�M�AhsA|  Ay��AvĜAtI�Aq��Ao��Ao%An$�Ak?}Ad��A_��A^�A]x�A\��A\��A\��A\�+AY�TAVn�ASp�AQ`BAO��ANZAM��AMC�AL��AK�FAKAI�
AHz�AF�`AF1AE|�AE
=ADȴADA�AC��ABĜAA?}A?�wA?A>��A>-A<ZA9"�A6�A4ĜA2VA1��A1`BA0�/A-t�A)�;A'�;A&ZA%K�A#�-A"^5A jAA��At�A��A�HA��A`BA+A�/AE�A�AC�A�A��A`BA�A�AdZAz�AbA��A�A�yA��AjA��A��A�A�9A(�A"�AffA+A
�jA
��A
^5A	x�A�RAA�A�wA/AVA��A�A��Al�A�\A|�A �@��F@�@��@�ff@���@��@���@�@� �@�R@�%@�F@��@�?}@�o@�5?@���@�j@�K�@�@�@�1@�+@���@�1@�;d@���@�@� �@۾w@�=q@ٺ^@��@�bN@��@֟�@�J@��@��@�@�J@ղ-@�r�@ӍP@�+@��@�ȴ@ҏ\@�`B@�n�@�Ĝ@�1'@˾w@��@�~�@�5?@�J@ə�@�&�@ȼj@�bN@Ǯ@�-@���@�-@�$�@Ĵ9@§�@��@���@���@���@�1'@�b@��F@�\)@��R@�n�@��@���@�j@���@��@�5?@��-@��7@���@��P@�@���@�^5@��@���@��@�Q�@� �@�I�@�Q�@�z�@�V@�ƨ@�n�@�dZ@�j@���@� �@�"�@��@��@�b@�o@���@�n�@���@�G�@���@��@��m@��;@��w@�ƨ@��;@��@���@�{@�O�@�bN@�|�@�"�@�33@�
=@��H@�ȴ@���@�-@�{@���@�z�@��F@�\)@�+@��@���@�M�@�O�@���@���@�1'@�1'@�j@���@�hs@��7@��j@��P@�;d@��@��R@�E�@�@�@�x�@�`B@��@��/@�j@�  @��@��@�@��\@�$�@��@���@��-@�7L@�%@��j@��@�Z@�A�@� �@��@��@���@�l�@�^5@��#@��-@�x�@���@�bN@�  @�  @��;@�l�@�l�@��@�
=@��y@�v�@�ff@���@�V@�O�@�%@�%@��`@��j@���@��@�bN@�I�@�9X@�1'@�b@��@��w@���@��@�t�@�\)@�\)@�S�@�;d@�@���@���@�@�o@�+@�"�@��H@��R@���@���@���@��\@�V@��T@��h@�x�@�G�@��@�9X@�t�@�\)@�\)@�K�@�;d@�@��H@�ȴ@���@�ff@�^5@��@��^@�x�@�G�@�&�@��9@�@�w@l�@
=@}�@}�@}�h@}O�@}O�@}?}@}/@}�@|��@|�@|��@|��@{��@{�@{S�@{33@z�H@z�!@zM�@zJ@y��@yG�@x��@xbN@w�@w�@w�P@w\)@w
=@v��@v�@v��@vff@vE�@u�-@u`B@t�@tI�@s�
@sdZ@r�@r~�@r��@r=q@q�#@qX@p�9@o�;@o|�@o;d@n�@n��@nff@m�@m�@m?}@l�@l(�@k�m@k�F@kC�@j�@j��@j~�@j~�@j��@k@k@j��@j~�@i��@i�7@ihs@h�9@g��@f�R@f�+@fV@f$�@e��@d��@d�@d(�@c�
@cdZ@co@b�H@b�H@b��@bn�@a��@a��@a�7@aG�@`�9@_�@_�@_��@_��@_|�@_\)@_+@^�@^ff@]@]`B@\9X@[ƨ@[�F@[��@[�@[C�@["�@Z�@Z��@Zn�@Z-@Y��@YX@Y&�@X��@XĜ@XA�@X  @W�;@W�w@W�P@W+@V�+@U��@U/@T�/@T�j@T�@TI�@S�F@SC�@R�@R��@R�\@R�\@R��@RM�@R�@Q��@PĜ@Pr�@O�@O�P@Ol�@O\)@O+@N��@Nv�@N@M�-@MO�@M�@L�@L��@L��@L��@LZ@LI�@L(�@K�
@K��@KS�@K"�@J�H@J=q@I�#@I�7@I7L@HĜ@Hr�@Hb@G�@Gl�@G;d@F��@Fv�@F$�@F{@E�h@EO�@D�@Dj@C�
@C��@C��@C�@Ct�@Co@B��@B��@Bn�@Bn�@B^5@A�7@A�@@��@@b@?�@?|�@?K�@?;d@?+@?�@>�@>�R@>��@>v�@=�@=`B@=�@<��@<�D@<I�@<(�@;��@;ƨ@;t�@;S�@;33@;o@;@:��@:-@9x�@8��@8�9@8��@8�u@8�u@8�@8Q�@8 �@8  @7��@7;d@6��@6�+@6$�@5�@5�T@5@5p�@4�/@4�D@4z�@4Z@49X@3��@3�F@3t�@3dZ@3S�@333@3o@2��@2^5@1��@1�@1��@1hs@1�@0��@0��@0  @/�w@/�P@/l�@/l�@/l�@/;d@/;d@/+@/�@/�@.�R@.5?@.$�@.$�@-��@-?}@-V@,�j@,9X@+�
@*�@)�#@)��@)X@)7L@)&�@)�@(�`@(Ĝ@(�9@(�9@(�u@(Q�@'�@'l�@'�@&�@&�R@&�R@&��@&��@&��@&v�@&V@&@%@%�-@%�h@%�@%p�@%O�@%/@%�@$�/@$�D@$9X@#��@#"�@"��@"��@"n�@"=q@!��@!7L@!%@ ��@ ��@ �u@ �u@ bN@�@��@\)@+@ȴ@�+@V@@@/@�D@Z@9X@�@�m@t�@C�@33@"�@�@�!@��@~�@n�@=q@J@�#@��@��@x�@x�@G�@�@�`@�u@1'@ �@ �@b@  @�@��@�@|�@;d@+@�@v�@E�@5?@�@�T@��@�@`B@�@�@��@Z@1@�m@�
@��@dZ@��@~�@��@X@X@X@X@X@X@X@7L@%@��@�`@�`@Ĝ@�9@�@r�@b@  @�@|�@K�@��@�@ȴ@E�@�T@@p�@�@�j@z�@z�@Z@�m@�F@��@��@�@�@t�@dZ@S�@33@
�@
�\@
^5@
M�@
=q@
-@	��@	��@	��@	��@	��@	�7@	�7@	��@	�7@	�7@	x�@	G�@	7L@	&�@	�@	%@��@Ĝ@Ĝ@�9@�u@�@bN@1'@ �@b@b@�;@�w@��@l�@+@�@�@�@�@�+@E�@E�@5?@{@�@@��@�@`B@?}@V@��@��@�@�@��@��@j@Z@I�@9X@�m@�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB�BB�BA�BA�BA�B@�B@�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�B@�B>wB=qB<jB:^B49B33B@�BI�BK�BI�BI�BM�BN�BM�BP�BR�BXBjBl�Bl�Bp�Bn�Bk�Bl�Bl�Bl�Bl�Bl�Bk�BgmBe`B`BB^5B^5B[#BVBM�BG�B=qB-B!�B�B�BuBbB
=BB��B��B�B�NB�B�
B��BȴB�!B��B�bB�1Bz�Br�BgmB]/BP�BG�B9XB,B�B
��B
�fB
�B
ɺB
��B
�FB
�B
��B
��B
�bB
� B
t�B
k�B
I�B
2-B
 �B
PB	��B	�B	�;B	�#B	��B	�jB	��B	|�B	v�B	q�B	n�B	l�B	k�B	hsB	ZB	G�B	9XB	/B	'�B	 �B	�B	�B	�B	uB	\B	
=B	B��B��B��B��B��B�B�B�B�`B�NB�BB�5B�B��BȴB�qB�RB�'B�B��B��B��B�DB�%B�B~�B}�B}�B~�B~�B}�B|�Bz�Bx�By�Bz�B{�By�Bw�Bv�Bu�Bu�Bt�Bs�Br�Bq�Bq�Bs�By�Bz�Bz�By�Bx�Bv�Bv�Bt�Br�Bp�Bo�Bm�Bl�Bk�BjBjBiyBiyBiyBhsBgmBffBdZBcTBbNBaHB_;B]/B\)B[#BZBYBXBYBYBXBXBXBZBZB[#B\)B\)B\)B]/B]/B\)B\)B]/B]/B]/B]/B^5B_;B`BBaHB`BBaHBdZBcTBffBffBhsBiyBiyBo�Bs�Bu�Bw�Bx�By�Bz�Bx�Bv�Bt�Bt�Bt�Bs�Bp�Bk�Bm�Bs�Bv�Bw�By�Bz�By�By�B{�B� B�B�B� B�B�B�B|�Bu�Bp�Bm�Bp�Bp�Bq�Br�Br�Br�Br�Br�Br�Bt�Bu�Bu�Bv�Bz�B|�B}�B� B�B�1B�7B�DB�VB�\B�VB�bB��B��B�-B�^BƨBĜBB��B�#B�5B�;B�HB�TB�ZB�`B�mB�B�B�B�B��B	  B	B	%B		7B	
=B	PB	bB	hB	�B	�B	�B	�B	 �B	"�B	#�B	%�B	%�B	'�B	)�B	,B	-B	.B	/B	1'B	33B	49B	5?B	6FB	7LB	9XB	;dB	?}B	A�B	F�B	I�B	L�B	O�B	Q�B	XB	XB	XB	XB	YB	[#B	\)B	\)B	\)B	]/B	]/B	^5B	`BB	`BB	`BB	cTB	e`B	gmB	hsB	hsB	iyB	m�B	m�B	o�B	r�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	r�B	q�B	s�B	u�B	w�B	{�B	~�B	�B	�B	�%B	�7B	�7B	�7B	�=B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�?B	�LB	�^B	�dB	�qB	��B	B	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�)B	�)B	�#B	�/B	�5B	�HB	�TB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
DB
VB
VB
\B
hB
hB
uB
{B
{B
{B
{B
uB
uB
uB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
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
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BB�BB�BA�BA�BA�B@�B@�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�B@�B>wB=qB<jB:^B49B33B@�BI�BK�BI�BI�BM�BN�BM�BP�BR�BXBjBl�Bl�Bp�Bn�Bk�Bl�Bl�Bl�Bl�Bl�Bk�BgmBe`B`BB^5B^5B[#BVBM�BG�B=qB-B!�B�B�BuBbB
=BB��B��B�B�NB�B�
B��BȴB�!B��B�bB�1Bz�Br�BgmB]/BP�BG�B9XB,B�B
��B
�fB
�B
ɺB
��B
�FB
�B
��B
��B
�bB
� B
t�B
k�B
I�B
2-B
 �B
PB	��B	�B	�;B	�#B	��B	�jB	��B	|�B	v�B	q�B	n�B	l�B	k�B	hsB	ZB	G�B	9XB	/B	'�B	 �B	�B	�B	�B	uB	\B	
=B	B��B��B��B��B��B�B�B�B�`B�NB�BB�5B�B��BȴB�qB�RB�'B�B��B��B��B�DB�%B�B~�B}�B}�B~�B~�B}�B|�Bz�Bx�By�Bz�B{�By�Bw�Bv�Bu�Bu�Bt�Bs�Br�Bq�Bq�Bs�By�Bz�Bz�By�Bx�Bv�Bv�Bt�Br�Bp�Bo�Bm�Bl�Bk�BjBjBiyBiyBiyBhsBgmBffBdZBcTBbNBaHB_;B]/B\)B[#BZBYBXBYBYBXBXBXBZBZB[#B\)B\)B\)B]/B]/B\)B\)B]/B]/B]/B]/B^5B_;B`BBaHB`BBaHBdZBcTBffBffBhsBiyBiyBo�Bs�Bu�Bw�Bx�By�Bz�Bx�Bv�Bt�Bt�Bt�Bs�Bp�Bk�Bm�Bs�Bv�Bw�By�Bz�By�By�B{�B� B�B�B� B�B�B�B|�Bu�Bp�Bm�Bp�Bp�Bq�Br�Br�Br�Br�Br�Br�Bt�Bu�Bu�Bv�Bz�B|�B}�B� B�B�1B�7B�DB�VB�\B�VB�bB��B��B�-B�^BƨBĜBB��B�#B�5B�;B�HB�TB�ZB�`B�mB�B�B�B�B��B	  B	B	%B		7B	
=B	PB	bB	hB	�B	�B	�B	�B	 �B	"�B	#�B	%�B	%�B	'�B	)�B	,B	-B	.B	/B	1'B	33B	49B	5?B	6FB	7LB	9XB	;dB	?}B	A�B	F�B	I�B	L�B	O�B	Q�B	XB	XB	XB	XB	YB	[#B	\)B	\)B	\)B	]/B	]/B	^5B	`BB	`BB	`BB	cTB	e`B	gmB	hsB	hsB	iyB	m�B	m�B	o�B	r�B	s�B	s�B	s�B	s�B	s�B	s�B	s�B	r�B	q�B	s�B	u�B	w�B	{�B	~�B	�B	�B	�%B	�7B	�7B	�7B	�=B	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�?B	�LB	�^B	�dB	�qB	��B	B	ĜB	ĜB	ĜB	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�)B	�)B	�#B	�/B	�5B	�HB	�TB	�ZB	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
%B
%B
+B
+B
1B
	7B
	7B
DB
VB
VB
\B
hB
hB
uB
{B
{B
{B
{B
uB
uB
uB
oB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
-B
-B
-B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
33B
5?B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
J�B
K�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
W
B
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
[#B
[#B
[#B
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
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.22 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20191008220030                              AO  ARCAADJP                                                                    20191008220030    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20191008220030  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20191008220030  QCF$                G�O�G�O�G�O�0               