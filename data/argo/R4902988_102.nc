CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-07-29T00:42:16Z creation;2022-07-29T00:42:16Z conversion to V3.1      
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220729004216  20220729010814  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               fA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @��U0[�1   @��VW��@;1hr� ��c�~��"�1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  @���AffA@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B ffB  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C�fC�fC  C�fC
  C  C�fC  C  C�C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cu�fCx�Cz  C{�fC~  C��C�  C�  C��C��C�  C��3C��3C�  C��C�  C��3C��3C�  C��C��C��C��C�  C��C��C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C��C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C��C��C��C�  C��3C��3C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C��C��C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D��D� D  D� D  D� D��D� D  D� DfD�fD  D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\�fD]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Dby�Dc  Dc� Dd  Ddy�Dd��De� Df  Df� Dg  Dg� Dh  Dh� DifDi�fDj  Dj� Dk  Dky�Dl  Dl� DmfDm� Dn  Dn� Do  Do�fDp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDufDu� Dv  Dv� Dw  Dw� Dw��Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D��3D�  D�@ D�� D�� D�3D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D���D�  D�<�D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D��3D�  D�<�D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�<�D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�<�D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ Dȼ�D�  D�C3Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΃3D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D׼�D�  D�@ D؀ D�� D�  D�@ Dك3D�� D�  D�<�Dڀ D��3D�  D�<�Dۀ D�� D�  D�C3D܀ Dܼ�D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�C3D� D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�C3D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@�@��\AG�A>�HA^�HA~�HA�p�A�p�A�p�A���A�p�A�p�A�p�B �B�RB�RB�RB�RB'�RB0�B7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB���B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B�\B�\B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)C�zC�zC�C�zC	�C�C�zC�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C<�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�zCi�Ck�Cm�Co�Cq�Cs�Cu�zCx�Cy�C{�zC}�C��C��
C��
C��C��C��
C��=C��=C��
C��C��
C��=C��=C��
C��C��C��C��C��
C��C��C��
C��
C��=C��
C��
C��
C��
C��=C��
C��C��
C��
C��
C��C��
C��
C��
C��C��
C��
C��
C��=C��
C��
C��
C��
C��
C��
C��
C��
C��C��
C��=C��=C��
C��C��
C��
C��
C��C��
C��
C��
C��C��
C��
C��
C��
C��
C��C��
C��
C��
C��
C��C��
C��
C��
C��=C��
C��
C��C��C��C��
C��=C��=C��
C��
C��C��C��
C��
C��
C��
C��
C��C��
C��
C��=C��
C��
C��C��C��C��C��
C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D �D {�D ��D{�D�D{�D��D{�D��D{�D�D{�D��D{�D�D��D��D{�D��D	{�D	��D
{�D
��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D��D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-�D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DH�DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ��DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�D[�D[{�D[��D\��D\��D]uD]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��DbuDb��Dc{�Dc��DduDd�De{�De��Df{�Df��Dg{�Dg��Dh{�Di�Di��Di��Dj{�Dj��DkuDk��Dl{�Dm�Dm{�Dm��Dn{�Dn��Do��Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt��Du�Du{�Du��Dv{�Dv��Dw{�Dw�Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D}�D}��D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D���D���D���D�=�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�:�D�}�D���D� �D�@�D�}�D���D���D�=�D�}�D���D� �D�@�D�}�D���D���D�:�D�}�D���D���D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�:�D�z�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�z�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�@�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�z�D���D���D�=�D�}�D���D���D�:�D�z�D���D���D�=�D�}�D���D���D�=�D�z�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�z�D���D���D�=�D�}�D���D���D�:�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȺ�D���D�@�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D΀�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֺ�D���D�=�D�}�D׺�D���D�=�D�}�Dؽ�D���D�=�Dـ�Dٽ�D���D�:�D�}�D���D���D�:�D�}�D۽�D���D�@�D�}�Dܺ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D� �D�=�D�}�D��D���D�=�D�z�D��D���D�=�D�}�D��D���D�=�D�z�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�:�D�}�D��D���D�=�D�}�D���D���D�:�D�z�D��D���D�@�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�:�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D� �D�@�D�}�D���D���D�:�D�z�D���D���D�=�D�}�D���D���D�:�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�[�A�e,A�gA�q�A�pA�p�A�x�A�|�A��AҀ A҄�A�y	A�\�A�A��%A��A��QA���A���AѼ�A���A���AͭCA��AÝ�A�iDA��7A�i�A�;dA��$A��?A���A�R�A���A�%A�̘A��bA�y�A�[�A�+6A���A�
�A��.A�W?A�b�A��A��?A��SA�S�A��A���A��]A��A�K�A��FA��A��A�~(A��A�FA�	7A���A��rA�(XA���A��A��fA�˒A�0�A�sMA�GA��)A��NA�˒A�� A��A�5?A�uA��dA��<A���A��LA��6A��*A�`A�:�A��A� �A�L0A��A��3A�HKA��qA��A��DA���A��A��KA��A���A��7A���A�ɆA���A���A1A|5?AzѷAy�DAx<6AuArیAq��Ao��Am��Ak�]Ak�Aj:�Ah�yAg33Afc AedZAc��AbH�A`�A_$A^�A\��A[A AYjAWs�AV��AU�AS�ARv�AQc�ANںAM:�ALl�AK��AJ�hAH�MAE�PADc�ACMAB��AB˒AB�hAA��A@5�A>�	A=�KA;��A:�4A9�A9xlA9#:A8��A7�9A6�jA5͟A5c�A5=A4�A4?�A3`BA3($A2�A1��A0�A0�A/�!A/cA/9XA.TaA-�SA,2aA+|A+oA*�A)jA(�]A(e,A'XyA&��A%�VA%!A$��A#2aA"w2A"�A!t�A ��A :*A�AtTA��A�A}�A��A&�A�A�A��A�fA�RA�A��AA�Au�A�A)_AVmAxAYKA��A��Ar�AZ�A
��A
��A
-wA	�A�rAzxAPHA�\A�A��A�AیAV�AO�ADgAp�A �_@��K@��@�+@��@��@��@���@��@�{@�U2@���@�ԕ@��@�@�@�X�@�%@��]@��@�v`@���@�w@��@�v�@�� @�Q�@��'@��g@�@@�'R@ݔ�@٨X@�E9@�S�@��/@���@�.�@�Y�@��@�1�@ΐ.@�_�@�Q@�{@�H�@�q@�Xy@���@Ȏ�@�m�@�c @�Z�@�� @�?�@ŭC@�9�@��v@ĥz@�M�@�e�@��@�� @���@��@�L�@�@��@��@�C�@�+@��f@��,@��1@���@�#:@�(@��=@�w2@���@��@��o@���@��f@�h
@���@��	@�@��@��@���@��@��C@�/�@���@���@���@��Y@�D�@��N@���@�iD@�=@�L0@���@�J�@��@���@��@�8�@��@�V@���@�0U@�g�@��O@�C-@�0U@�$@�u@��d@�!�@��@��/@�)�@���@�j@�?}@���@�;d@��`@��u@�Z@�-@��W@���@�S&@��X@��1@�:�@��@�|@�S&@�+@�*0@�8@�ی@�h�@��@��'@�%@���@��.@�z�@�]d@�9X@�.�@���@��@���@��X@�l�@��`@���@��@�V@���@�*0@���@���@��4@��r@�q�@�c @�a|@�ff@�4�@�n�@�;�@�
�@��Q@��:@�H�@��@��v@���@���@��z@�p;@�Xy@�M@��@�J@���@��"@� i@���@�?@��9@��$@�~�@�s�@�f�@�C�@��@���@���@���@�~�@�p;@�&�@���@��@���@���@�}�@�?}@�+@��@���@�H@�	�@�*@~�@~�A@~5?@}�H@}+@|�5@|�@zGE@yk�@yN<@y-w@x��@xN�@x?�@x�@wK�@v}V@v@u�7@t�5@toi@t�@s�@rE�@q2a@p�@p�o@p>B@o�g@oRT@oC@o�@n��@n�B@n�'@n��@n��@n��@nkQ@nu@mw2@m%F@m@@l��@lD�@l�@l7@l<�@l7@k�@@k,�@k$t@k�@j�@j}V@j{@i^�@i@@h��@h��@h"h@h~@g�&@f�R@f_�@fW�@fC�@f?@e�9@e��@e\�@e+@d�5@d�@d�@d�@d�_@d�@d@c��@c�@bں@cA�@cZ�@cs@co�@c(@b�1@b:*@aԕ@ao @`�	@`��@`��@`U2@`2�@`�@_��@^��@^W�@^�@^u@]��@\��@\��@\�u@\x@[x@Z�6@Y��@YO�@XH@W�0@W�@W{J@W6z@V�@U�'@U<6@T[�@S�+@S�@S�@S�4@Sl�@Sa@S@O@R�8@R��@R�@S1�@R�"@Rc @R �@Q�X@Q/@Pی@Pj@PXy@PV�@PU2@PQ�@P �@O�@O~�@OP�@O4�@O@N��@N͟@Nq�@M�-@Mj@M0�@M	l@L�@L�j@L�@Lc�@LM@L�@K�+@Kƨ@K��@K@JOv@I��@I�@H��@H��@H�_@H��@H�@H�@H�@H�@Hu�@HS�@H�@Gخ@G�V@Gj�@G;d@F�c@FYK@E��@E�M@E!�@D��@D�@D֡@D�?@D�U@D��@D�$@D�O@D�_@DI�@C�a@Cb�@B�m@BL0@B�@Ahs@@�P@@�j@@��@@Q�@@@?�
@?J#@>;�@=�^@=��@=F@=2a@=@<��@<<�@;�r@;��@;��@;s@;$t@:�@:�<@:z@:H�@9��@9u�@9L�@9@@8��@8�@8�O@8j@86@7�@7�4@7\)@74�@7�@6�"@6��@6�\@6ff@6�@5�7@4Ɇ@4C-@3�@@3P�@3�@2�]@2h
@1�.@1�@1�@1�@1�@1a�@0�	@0�4@0S�@0~@/�
@/��@/W?@/8@.�"@.�r@.Ov@.+k@._@-��@-^�@-0�@-	l@,�v@,��@,`�@,1@+y�@+/�@*ߤ@*�<@*�@*��@*	@)�@)5�@)�@(�p@(��@(�U@(��@(m�@'�@'o�@'W?@'E9@'6z@'!-@&�M@&�@&q�@&J@%�@%�^@%��@%��@%�"@%�h@%x�@%G�@%*0@$֡@$��@$��@$�@$�j@$��@$��@$q@#��@#�{@#)_@"�8@"��@"ں@"�'@"��@"i�@"u@!�@!�C@!��@!�=@!o @!=�@!(�@!�@ �K@ ��@ Ft@ �@��@ƨ@��@��@�4@;d@+@&@��@��@\�@&�@�@�@�-@�S@}�@f�@X@Q�@A @�@�P@�	@�$@K^@�@�;@�g@��@��@�f@iD@>�@S@�R@��@l�@)�@�T@�N@�@�h@�@��@�@	�@	�@�]@�@��@� @�0@��@�@g�@6z@�@��@ں@͟@�R@�L@��@�r@�r@��@;�@�P@��@�@Xy@K^@7�@:�@7�@�]@��@��@33@�"@��@��@҉@��@��@Z�@C�@1�@!�@�.@�@f�@��@l"@A�@�@�+@�F@S�@P�@Mj@!-@�@�X@�A@L0@
�@��@�@�9@�3@�z@��@�N@�^@�7@L�@?}@�@%@Ĝ@�D@u�@oi@PH@Ft@:�@,=@�@��@�g@��@�4@RT@;d@'�@
�@
�@
�X@
��@
�L@
�A@
^5@
Z�@
Q@
E�@
?@
#:@	�@	�#@	�^@	��@	��@	L�@	7L@	(�@	&�@	#�@ی@��@��@h�@7�@�@�W@��@�@�0@��@6z@�y@��@��@�\@�A@s�@Ov@;�@)�@�@��@�>@��@Dg@!�@��@��@w�@1'@@�g@�:@�f@{J@j�@H�@�@�2@z@C�@)�@&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�[�A�e,A�gA�q�A�pA�p�A�x�A�|�A��AҀ A҄�A�y	A�\�A�A��%A��A��QA���A���AѼ�A���A���AͭCA��AÝ�A�iDA��7A�i�A�;dA��$A��?A���A�R�A���A�%A�̘A��bA�y�A�[�A�+6A���A�
�A��.A�W?A�b�A��A��?A��SA�S�A��A���A��]A��A�K�A��FA��A��A�~(A��A�FA�	7A���A��rA�(XA���A��A��fA�˒A�0�A�sMA�GA��)A��NA�˒A�� A��A�5?A�uA��dA��<A���A��LA��6A��*A�`A�:�A��A� �A�L0A��A��3A�HKA��qA��A��DA���A��A��KA��A���A��7A���A�ɆA���A���A1A|5?AzѷAy�DAx<6AuArیAq��Ao��Am��Ak�]Ak�Aj:�Ah�yAg33Afc AedZAc��AbH�A`�A_$A^�A\��A[A AYjAWs�AV��AU�AS�ARv�AQc�ANںAM:�ALl�AK��AJ�hAH�MAE�PADc�ACMAB��AB˒AB�hAA��A@5�A>�	A=�KA;��A:�4A9�A9xlA9#:A8��A7�9A6�jA5͟A5c�A5=A4�A4?�A3`BA3($A2�A1��A0�A0�A/�!A/cA/9XA.TaA-�SA,2aA+|A+oA*�A)jA(�]A(e,A'XyA&��A%�VA%!A$��A#2aA"w2A"�A!t�A ��A :*A�AtTA��A�A}�A��A&�A�A�A��A�fA�RA�A��AA�Au�A�A)_AVmAxAYKA��A��Ar�AZ�A
��A
��A
-wA	�A�rAzxAPHA�\A�A��A�AیAV�AO�ADgAp�A �_@��K@��@�+@��@��@��@���@��@�{@�U2@���@�ԕ@��@�@�@�X�@�%@��]@��@�v`@���@�w@��@�v�@�� @�Q�@��'@��g@�@@�'R@ݔ�@٨X@�E9@�S�@��/@���@�.�@�Y�@��@�1�@ΐ.@�_�@�Q@�{@�H�@�q@�Xy@���@Ȏ�@�m�@�c @�Z�@�� @�?�@ŭC@�9�@��v@ĥz@�M�@�e�@��@�� @���@��@�L�@�@��@��@�C�@�+@��f@��,@��1@���@�#:@�(@��=@�w2@���@��@��o@���@��f@�h
@���@��	@�@��@��@���@��@��C@�/�@���@���@���@��Y@�D�@��N@���@�iD@�=@�L0@���@�J�@��@���@��@�8�@��@�V@���@�0U@�g�@��O@�C-@�0U@�$@�u@��d@�!�@��@��/@�)�@���@�j@�?}@���@�;d@��`@��u@�Z@�-@��W@���@�S&@��X@��1@�:�@��@�|@�S&@�+@�*0@�8@�ی@�h�@��@��'@�%@���@��.@�z�@�]d@�9X@�.�@���@��@���@��X@�l�@��`@���@��@�V@���@�*0@���@���@��4@��r@�q�@�c @�a|@�ff@�4�@�n�@�;�@�
�@��Q@��:@�H�@��@��v@���@���@��z@�p;@�Xy@�M@��@�J@���@��"@� i@���@�?@��9@��$@�~�@�s�@�f�@�C�@��@���@���@���@�~�@�p;@�&�@���@��@���@���@�}�@�?}@�+@��@���@�H@�	�@�*@~�@~�A@~5?@}�H@}+@|�5@|�@zGE@yk�@yN<@y-w@x��@xN�@x?�@x�@wK�@v}V@v@u�7@t�5@toi@t�@s�@rE�@q2a@p�@p�o@p>B@o�g@oRT@oC@o�@n��@n�B@n�'@n��@n��@n��@nkQ@nu@mw2@m%F@m@@l��@lD�@l�@l7@l<�@l7@k�@@k,�@k$t@k�@j�@j}V@j{@i^�@i@@h��@h��@h"h@h~@g�&@f�R@f_�@fW�@fC�@f?@e�9@e��@e\�@e+@d�5@d�@d�@d�@d�_@d�@d@c��@c�@bں@cA�@cZ�@cs@co�@c(@b�1@b:*@aԕ@ao @`�	@`��@`��@`U2@`2�@`�@_��@^��@^W�@^�@^u@]��@\��@\��@\�u@\x@[x@Z�6@Y��@YO�@XH@W�0@W�@W{J@W6z@V�@U�'@U<6@T[�@S�+@S�@S�@S�4@Sl�@Sa@S@O@R�8@R��@R�@S1�@R�"@Rc @R �@Q�X@Q/@Pی@Pj@PXy@PV�@PU2@PQ�@P �@O�@O~�@OP�@O4�@O@N��@N͟@Nq�@M�-@Mj@M0�@M	l@L�@L�j@L�@Lc�@LM@L�@K�+@Kƨ@K��@K@JOv@I��@I�@H��@H��@H�_@H��@H�@H�@H�@H�@Hu�@HS�@H�@Gخ@G�V@Gj�@G;d@F�c@FYK@E��@E�M@E!�@D��@D�@D֡@D�?@D�U@D��@D�$@D�O@D�_@DI�@C�a@Cb�@B�m@BL0@B�@Ahs@@�P@@�j@@��@@Q�@@@?�
@?J#@>;�@=�^@=��@=F@=2a@=@<��@<<�@;�r@;��@;��@;s@;$t@:�@:�<@:z@:H�@9��@9u�@9L�@9@@8��@8�@8�O@8j@86@7�@7�4@7\)@74�@7�@6�"@6��@6�\@6ff@6�@5�7@4Ɇ@4C-@3�@@3P�@3�@2�]@2h
@1�.@1�@1�@1�@1�@1a�@0�	@0�4@0S�@0~@/�
@/��@/W?@/8@.�"@.�r@.Ov@.+k@._@-��@-^�@-0�@-	l@,�v@,��@,`�@,1@+y�@+/�@*ߤ@*�<@*�@*��@*	@)�@)5�@)�@(�p@(��@(�U@(��@(m�@'�@'o�@'W?@'E9@'6z@'!-@&�M@&�@&q�@&J@%�@%�^@%��@%��@%�"@%�h@%x�@%G�@%*0@$֡@$��@$��@$�@$�j@$��@$��@$q@#��@#�{@#)_@"�8@"��@"ں@"�'@"��@"i�@"u@!�@!�C@!��@!�=@!o @!=�@!(�@!�@ �K@ ��@ Ft@ �@��@ƨ@��@��@�4@;d@+@&@��@��@\�@&�@�@�@�-@�S@}�@f�@X@Q�@A @�@�P@�	@�$@K^@�@�;@�g@��@��@�f@iD@>�@S@�R@��@l�@)�@�T@�N@�@�h@�@��@�@	�@	�@�]@�@��@� @�0@��@�@g�@6z@�@��@ں@͟@�R@�L@��@�r@�r@��@;�@�P@��@�@Xy@K^@7�@:�@7�@�]@��@��@33@�"@��@��@҉@��@��@Z�@C�@1�@!�@�.@�@f�@��@l"@A�@�@�+@�F@S�@P�@Mj@!-@�@�X@�A@L0@
�@��@�@�9@�3@�z@��@�N@�^@�7@L�@?}@�@%@Ĝ@�D@u�@oi@PH@Ft@:�@,=@�@��@�g@��@�4@RT@;d@'�@
�@
�@
�X@
��@
�L@
�A@
^5@
Z�@
Q@
E�@
?@
#:@	�@	�#@	�^@	��@	��@	L�@	7L@	(�@	&�@	#�@ی@��@��@h�@7�@�@�W@��@�@�0@��@6z@�y@��@��@�\@�A@s�@Ov@;�@)�@�@��@�>@��@Dg@!�@��@��@w�@1'@@�g@�:@�f@{J@j�@H�@�@�2@z@C�@)�@&�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B%�B&�B'�B(sB)_B)�B)�B)�B)�B(�B(sB'�B(
B($B'�B'B&B%�B$ZB �BB
�B��BуB��B��B��B��B��B�KB��B�bB��B�B�B��B��B��B�FB��B��B�:B�;B��B��B��B��B�B��B�B��B��B�BzBr�BlBc�BW�BS�BI�B<�B5%B3�B0UB!�B�B~B�B�qB�2B�,B��B��B��B�qB�?B��B�B��B��B�hB�B�KB{BkBVSBN"BBAB/�BYB�B
XB �B��B��B�UB�`B��B�DBy	Bg8B[=BL�B8�B-�BB�B�B
�B
��B
��B
�0B
�[B
��B
��B
��B
�uB
��B
��B
��B
�iB
~]B
v+B
j�B
c�B
W�B
QNB
G�B
=�B
2�B
%�B
$�B
CB
TB
�B
�B	�B	�B	��B	�NB	�xB	��B	��B	�B	��B	�AB	�B	��B	��B	�fB	��B	��B	�B	�FB	��B	�TB	�}B	��B	�	B	�EB	��B	�B	�aB	� B	~�B	{�B	zxB	z*B	.B	�AB	~�B	HB	�iB	� B	|�B	y>B	tB	o5B	lqB	h>B	fB	b�B	b4B	^�B	]�B	_�B	_�B	]~B	X�B	TaB	T�B	S�B	Q�B	OB	MB	KDB	I�B	F�B	C�B	AUB	>�B	:xB	6B	2�B	,"B	(>B	%�B	#�B	"NB	 BB	�B	�B	1B	?B	�B	B	�B	DB	xB	�B	1B	+B	+B	�B	�B	-B	�B	 OB��B��B�<B��B��B��B�B�3B��B�B� B�IB�B�kB��B��B�B��B�B�hB�hB�4B�HB��B�vB�B��BބB�jB�jB�IB�B�xB�B��BںB�1B�BٚB�
B��B�
B��B�mB�gB�B��B�B՛B�MB�MB�MB�gB��B�YB�B��B�B�1B�B��B�	B�BܒBܒBیBۦB��B�/B�5B�VB�pB�|B� B�B��B�FB�FB�B��B�zB�2B�B�B��B��B�}B�-B�+B�JB�(B	�B	YB	^B	uB	�B	aB	�B	9B	�B	B	�B	pB	&2B	'�B	+�B	*�B	+kB	+�B	.cB	0;B	/�B	0!B	/iB	0B	2aB	2�B	2�B	5%B	;B	>(B	A�B	EmB	F�B	G�B	I7B	I�B	J�B	KxB	N�B	O\B	P�B	Q�B	R�B	UB	YKB	]�B	_�B	`�B	`�B	b4B	c:B	ezB	h�B	j0B	l�B	o�B	rGB	s�B	u�B	xB	z�B	�B	�B	�B	� B	��B	�B	�GB	��B	�MB	��B	�%B	�fB	��B	��B	��B	�:B	��B	�_B	�B	�=B	��B	�B	�B	�IB	��B	�B	�tB	��B	��B	��B	��B	�4B	�B	��B	��B	��B	ªB	��B	��B	ʌB	�.B	�{B	چB	�=B	��B	�;B	߾B	�B	�B	��B	�B	�B	�eB	�CB	�B	�iB	�;B	�B	��B	�rB	��B	��B	��B	�B
B
gB
�B
tB
�B
�B

#B

�B
�B
VB
hB
�B
{B
�B
�B
�B
7B
]B
]B
OB
!B
$ZB
%,B
&B
)�B
+�B
+kB
,=B
/B
1�B
2�B
5?B
7fB
8�B
:�B
<�B
AB
C{B
C{B
C�B
DB
E9B
F�B
G�B
IB
L�B
N�B
PHB
PHB
P�B
QB
QhB
S@B
U2B
V�B
W�B
Y�B
Z�B
[�B
^B
`B
`�B
a�B
c�B
c�B
d�B
d�B
g8B
jKB
l�B
mwB
m�B
oB
pUB
p;B
q�B
y�B
|�B
}<B
}qB
}�B
B
� B
�oB
��B
�-B
�B
��B
�B
�=B
�(B
��B
�oB
�:B
��B
�sB
�_B
�B
��B
��B
�5B
��B
��B
��B
�hB
��B
�:B
�TB
��B
��B
�zB
�mB
�DB
��B
��B
�B
�=B
��B
��B
��B
��B
�B
�GB
�B
�nB
�tB
��B
��B
��B
�+B
�B
��B
��B
��B
�lB
��B
��B
��B
�XB
�0B
��B
�HB
�OB
�B
�[B
�{B
��B
�SB
ƎB
ǔB
ȚB
��B
��B
��B
ȚB
�RB
�=B
��B
��B
�DB
��B
�JB
̘B
�B
�\B
ϫB
��B
�HB
ЗB
уB
҉B
��B
�B
өB
��B
�aB
��B
��B
��B
��B
�qB
�B
�B
�CB
�CB
�)B
�)B
�]B
�]B
�]B
�xB
�B
�IB
��B
�5B
�OB
�;B
�vB
�HB
�hB
�nB
�B
�B
�&B
�ZB
�@B
�ZB
�ZB
�tB
�B
�FB
�2B
�mB
��B
�B
�B
��B
�B
�B
�]B
��B
�B
�cB
�B
��B
�B
��B
�B
�B
�B
�B
�?B
��B
��B
��B
�zB
��B
�2B
��B
��B
��B
�>B
�rB
�*B
�xB
��B
��B
��B
�dB
��B
�PB
��B
�"B
�qB
��B
��B
�BB
�]B
��B
�HB
��B;B�BBaB�BBB�B�B�B�BBtB+B�BBKB�B	RB	�B	�B
	B
�BBDBxB�B~B�B�B6B�B�BVB(BvBB.B.B�BNBTB�B�B�B[B[B�B�BB2BgB�B�B�BB�B�B�B�B�B�BB+BBEB�B�BeBeBBeBBBeB�B�B	B�B�B�BBCB)B�BdB�B�B�B�BB�B�B�B�B�B�B B �B vB �B �B �B!bB!bB!bB"4B"hB"�B#B#:B#nB#�B#�B#�B$B$&B$@B$@B$�B$�B$�B$�B%�B%�B&B&B&2B&2B&�B&�B&�B'B'�B'�B'�B(sB(�B(�B(�B(�B)�B*KB+B+6B+6B+QB+kB+kB+�B+�B+�B,"B,=B,�B,�B,�B-)B-)B-CB-]B-�B-�B-�B-]B-�B/�B/�B0UB0�B0�B0�B0�B0�B1B1[B1�B2-B2|B2|B2|B2�B2�B2�B3MB3hB3�B3hB3�B3�B4�B5%B5�B5�B6B6FB6�B7LB7LB72B7�B7�B8B8RB8�B9$B9>B9>B9rB9�B9�B9�B9XB9�B9�B:DB:*B:xB:xB:�B;0B;dB;dB;�B;�B;�B;�B<B<B<PB<�B<�B=B="B="B=�B=�B=�B=�B>B>(B>wB>]B>�B>�B>�B>�B?B?.B?cB?cB?}B@ B@ B@B@B@B@�B@�BABA BAoBA�BA�BA�BA�BBBB'BB�BCGBC-BCaBC�BC�BC�BC�BDBDBD3BDMBDMBD�BESBE�BFBE�BFtBF�BF�BG_BG�BG�BG�BG�BG�BHBH�BI7BIlBIlBI�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B%�B&�B'�B(sB)_B)�B)�B)�B)�B(�B(sB'�B(
B($B'�B'B&B%�B$ZB �BB
�B��BуB��B��B��B��B��B�KB��B�bB��B�B�B��B��B��B�FB��B��B�:B�;B��B��B��B��B�B��B�B��B��B�BzBr�BlBc�BW�BS�BI�B<�B5%B3�B0UB!�B�B~B�B�qB�2B�,B��B��B��B�qB�?B��B�B��B��B�hB�B�KB{BkBVSBN"BBAB/�BYB�B
XB �B��B��B�UB�`B��B�DBy	Bg8B[=BL�B8�B-�BB�B�B
�B
��B
��B
�0B
�[B
��B
��B
��B
�uB
��B
��B
��B
�iB
~]B
v+B
j�B
c�B
W�B
QNB
G�B
=�B
2�B
%�B
$�B
CB
TB
�B
�B	�B	�B	��B	�NB	�xB	��B	��B	�B	��B	�AB	�B	��B	��B	�fB	��B	��B	�B	�FB	��B	�TB	�}B	��B	�	B	�EB	��B	�B	�aB	� B	~�B	{�B	zxB	z*B	.B	�AB	~�B	HB	�iB	� B	|�B	y>B	tB	o5B	lqB	h>B	fB	b�B	b4B	^�B	]�B	_�B	_�B	]~B	X�B	TaB	T�B	S�B	Q�B	OB	MB	KDB	I�B	F�B	C�B	AUB	>�B	:xB	6B	2�B	,"B	(>B	%�B	#�B	"NB	 BB	�B	�B	1B	?B	�B	B	�B	DB	xB	�B	1B	+B	+B	�B	�B	-B	�B	 OB��B��B�<B��B��B��B�B�3B��B�B� B�IB�B�kB��B��B�B��B�B�hB�hB�4B�HB��B�vB�B��BބB�jB�jB�IB�B�xB�B��BںB�1B�BٚB�
B��B�
B��B�mB�gB�B��B�B՛B�MB�MB�MB�gB��B�YB�B��B�B�1B�B��B�	B�BܒBܒBیBۦB��B�/B�5B�VB�pB�|B� B�B��B�FB�FB�B��B�zB�2B�B�B��B��B�}B�-B�+B�JB�(B	�B	YB	^B	uB	�B	aB	�B	9B	�B	B	�B	pB	&2B	'�B	+�B	*�B	+kB	+�B	.cB	0;B	/�B	0!B	/iB	0B	2aB	2�B	2�B	5%B	;B	>(B	A�B	EmB	F�B	G�B	I7B	I�B	J�B	KxB	N�B	O\B	P�B	Q�B	R�B	UB	YKB	]�B	_�B	`�B	`�B	b4B	c:B	ezB	h�B	j0B	l�B	o�B	rGB	s�B	u�B	xB	z�B	�B	�B	�B	� B	��B	�B	�GB	��B	�MB	��B	�%B	�fB	��B	��B	��B	�:B	��B	�_B	�B	�=B	��B	�B	�B	�IB	��B	�B	�tB	��B	��B	��B	��B	�4B	�B	��B	��B	��B	ªB	��B	��B	ʌB	�.B	�{B	چB	�=B	��B	�;B	߾B	�B	�B	��B	�B	�B	�eB	�CB	�B	�iB	�;B	�B	��B	�rB	��B	��B	��B	�B
B
gB
�B
tB
�B
�B

#B

�B
�B
VB
hB
�B
{B
�B
�B
�B
7B
]B
]B
OB
!B
$ZB
%,B
&B
)�B
+�B
+kB
,=B
/B
1�B
2�B
5?B
7fB
8�B
:�B
<�B
AB
C{B
C{B
C�B
DB
E9B
F�B
G�B
IB
L�B
N�B
PHB
PHB
P�B
QB
QhB
S@B
U2B
V�B
W�B
Y�B
Z�B
[�B
^B
`B
`�B
a�B
c�B
c�B
d�B
d�B
g8B
jKB
l�B
mwB
m�B
oB
pUB
p;B
q�B
y�B
|�B
}<B
}qB
}�B
B
� B
�oB
��B
�-B
�B
��B
�B
�=B
�(B
��B
�oB
�:B
��B
�sB
�_B
�B
��B
��B
�5B
��B
��B
��B
�hB
��B
�:B
�TB
��B
��B
�zB
�mB
�DB
��B
��B
�B
�=B
��B
��B
��B
��B
�B
�GB
�B
�nB
�tB
��B
��B
��B
�+B
�B
��B
��B
��B
�lB
��B
��B
��B
�XB
�0B
��B
�HB
�OB
�B
�[B
�{B
��B
�SB
ƎB
ǔB
ȚB
��B
��B
��B
ȚB
�RB
�=B
��B
��B
�DB
��B
�JB
̘B
�B
�\B
ϫB
��B
�HB
ЗB
уB
҉B
��B
�B
өB
��B
�aB
��B
��B
��B
��B
�qB
�B
�B
�CB
�CB
�)B
�)B
�]B
�]B
�]B
�xB
�B
�IB
��B
�5B
�OB
�;B
�vB
�HB
�hB
�nB
�B
�B
�&B
�ZB
�@B
�ZB
�ZB
�tB
�B
�FB
�2B
�mB
��B
�B
�B
��B
�B
�B
�]B
��B
�B
�cB
�B
��B
�B
��B
�B
�B
�B
�B
�?B
��B
��B
��B
�zB
��B
�2B
��B
��B
��B
�>B
�rB
�*B
�xB
��B
��B
��B
�dB
��B
�PB
��B
�"B
�qB
��B
��B
�BB
�]B
��B
�HB
��B;B�BBaB�BBB�B�B�B�BBtB+B�BBKB�B	RB	�B	�B
	B
�BBDBxB�B~B�B�B6B�B�BVB(BvBB.B.B�BNBTB�B�B�B[B[B�B�BB2BgB�B�B�BB�B�B�B�B�B�BB+BBEB�B�BeBeBBeBBBeB�B�B	B�B�B�BBCB)B�BdB�B�B�B�BB�B�B�B�B�B�B B �B vB �B �B �B!bB!bB!bB"4B"hB"�B#B#:B#nB#�B#�B#�B$B$&B$@B$@B$�B$�B$�B$�B%�B%�B&B&B&2B&2B&�B&�B&�B'B'�B'�B'�B(sB(�B(�B(�B(�B)�B*KB+B+6B+6B+QB+kB+kB+�B+�B+�B,"B,=B,�B,�B,�B-)B-)B-CB-]B-�B-�B-�B-]B-�B/�B/�B0UB0�B0�B0�B0�B0�B1B1[B1�B2-B2|B2|B2|B2�B2�B2�B3MB3hB3�B3hB3�B3�B4�B5%B5�B5�B6B6FB6�B7LB7LB72B7�B7�B8B8RB8�B9$B9>B9>B9rB9�B9�B9�B9XB9�B9�B:DB:*B:xB:xB:�B;0B;dB;dB;�B;�B;�B;�B<B<B<PB<�B<�B=B="B="B=�B=�B=�B=�B>B>(B>wB>]B>�B>�B>�B>�B?B?.B?cB?cB?}B@ B@ B@B@B@B@�B@�BABA BAoBA�BA�BA�BA�BBBB'BB�BCGBC-BCaBC�BC�BC�BC�BDBDBD3BDMBDMBD�BESBE�BFBE�BFtBF�BF�BG_BG�BG�BG�BG�BG�BHBH�BI7BIlBIlBI�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220729004124  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220729004216  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220729004216  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220729004216                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220729094221  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220729094221  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220729010814                      G�O�G�O�G�O�                