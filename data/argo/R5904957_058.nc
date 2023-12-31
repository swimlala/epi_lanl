CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:15Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140815  20181024140815  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               :A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׾$�uC1   @׾%@yoL@3�/���c����l�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      :A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A���A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C&  C(  C*  C,  C-�fC0  C2  C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D�fD  D� D  D� D   D � D ��D!y�D"  D"� D#fD#�fD$  D$� D%  D%� D%��D&y�D'  D'� D(fD(�fD)fD)� D*  D*y�D*��D+� D,  D,� D-fD-�fD.fD.� D.��D/y�D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>�fD?  D?y�D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DFy�DG  DG� DH  DHy�DI  DI�fDJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\fD\� D\��D]� D^  D^� D_fD_� D_��D`� Da  Da� Db  Db� Dc  Dc�fDdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn�fDo  Do� Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dw� Dw�3Dy�=D�AHD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�(�@�A�HA"�HAB�HAb�HA�p�A�p�A�p�A�=qA�p�A�p�A�p�A�p�B �RB�RB�RB�RB �RB(�RB0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B��\B��\B��\B�\)B��\B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C&.C(.C*.C,.C.zC0.C2.C4zC6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CL.CN.CP.CR.CT.CV.CX.CZ.C\.C^.C`.Cb.Cd.Cf.Ch.Cj.Cl.Cn.Cp.Cr.Ct.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�
=C�
C�
C�
C�#�C�#�C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�#�C�
C�
=C�
C�#�C�
C�
C�
C�
C�
=C�
=C�
C�
C�
=C�
D �D ��DD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D �D ��D!D!�D"�D"��D#�D#��D$�D$��D%�D%��D&D&�D'�D'��D(�D(��D)�D)��D*�D*�D+D+��D,�D,��D-�D-��D.�D.��D/D/�D0D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?�D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF�DG�DG��DH�DH�DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQDQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]D]��D^�D^��D_�D_��D`D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��DpDp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du�Dv�Dv��Dw�Dw��Dw޸Dy��D�GD�њ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���A���A���A��/A��#A���A��
A��
A���A���A���A���A���A���AۼjA�ƨA���A���A�ȴA۝�A�%A�VA�XA�bNA�bNA��Aٟ�A���A��A�oA�K�A��`Aӗ�A��A��/A�%A��A�I�A��
Aˏ\A���AʓuA�^5A� �A�bNA�/A���AȑhA�^5AǸRA���A�AŲ-Aş�AāA���A��A���A��#A�"�A�hsA� �A���A�hsA���A�|�A�VA�A�ZA���A���A�A�XA�O�A���A��A��RA�~�A�/A��jA���A�+A�A��RA��hA�+A��^A�"�A�&�A�E�A��DA�oA�JA/A}A{7LAx  AvffAt��Ar  Ao+Am��Al�9Ak��Aj�jAh��Agp�Ad�`AbbNA`�DA^ĜA[x�AW�mAU�wATv�AS&�AR��AR�AP��AN��AMC�AK�wAI33AF=qAD�ABM�A@{A=�A:��A7�A4A�A2�A01A/?}A.��A-�A*��A*M�A)\)A'ƨA'�A&�jA%VA$��A$^5A#�^A#7LA"��A"^5A"  A!\)A ��A JA33A�uA5?A��A�;A�wA��A�yA��A�!A�A�Ax�A{AAI�A9XA�uA�RAZAQ�A��A
�+A	C�A	�A��A33A`BA\)A�AM�A��A�/A�At�A�A ��A ~�A��A��A�-A��A�wAV@�I�@�x�A �@��-@���@��H@�o@�r�@�K�@�V@�j@�\)@��T@�X@�h@�bN@�+@��@���@���@�@��m@�/@ҏ\@�@ѡ�@ύP@�x�@���@̓u@���@���@š�@°!@�r�@��@�~�@�{@���@��y@þw@�V@�X@Ə\@�o@�b@ɺ^@��#@�O�@��@ɉ7@ɩ�@ɉ7@��@ǶF@��@ư!@�5?@��#@�G�@��@�(�@���@�S�@�hs@�r�@�9X@�Q�@�Q�@�1@��@��@�@��@��@��@�S�@�o@�ff@���@��h@��@��@���@��P@��@��P@��P@��@�"�@���@�$�@�x�@��@���@��@��@��j@�r�@�9X@��;@�33@���@�=q@���@���@�G�@�%@��`@��@��@�bN@�1'@��m@��w@��P@�l�@�ȴ@�v�@�ff@��T@�7L@��/@���@���@��@���@�t�@�S�@�33@�o@�@���@�v�@�^5@�M�@�-@�{@���@�G�@���@�z�@�Z@� �@�1@�  @���@��;@��
@�S�@��\@��@�hs@�&�@���@���@��D@�I�@��m@��@�o@��y@���@�-@�@��7@�X@�?}@���@���@��9@�z�@�I�@�1@�  @��m@��F@�l�@�C�@��R@�$�@��^@��h@�x�@�/@��@�Ĝ@��@�z�@�Q�@��;@��@��P@�|�@�|�@�;d@��@�l�@�
=@���@�bN@�1@��;@���@�l�@�K�@�+@�@�ȴ@���@�v�@�v�@�v�@�ff@���@�G�@�&�@�%@��@��`@���@��j@���@�I�@��@�K�@���@�V@�=q@��#@��@��@��/@�Ĝ@��u@�A�@��@��F@�\)@���@��!@���@��!@��!@��R@��+@�=q@�$�@��@�`B@���@���@��D@�Q�@���@��F@�S�@��@���@�E�@�-@�{@���@���@�O�@���@���@�I�@��@�t�@�o@��R@���@�~�@��K@x��@htT1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ƨA���A���A���A��/A��#A���A��
A��
A���A���A���A���A���A���AۼjA�ƨA���A���A�ȴA۝�A�%A�VA�XA�bNA�bNA��Aٟ�A���A��A�oA�K�A��`Aӗ�A��A��/A�%A��A�I�A��
Aˏ\A���AʓuA�^5A� �A�bNA�/A���AȑhA�^5AǸRA���A�AŲ-Aş�AāA���A��A���A��#A�"�A�hsA� �A���A�hsA���A�|�A�VA�A�ZA���A���A�A�XA�O�A���A��A��RA�~�A�/A��jA���A�+A�A��RA��hA�+A��^A�"�A�&�A�E�A��DA�oA�JA/A}A{7LAx  AvffAt��Ar  Ao+Am��Al�9Ak��Aj�jAh��Agp�Ad�`AbbNA`�DA^ĜA[x�AW�mAU�wATv�AS&�AR��AR�AP��AN��AMC�AK�wAI33AF=qAD�ABM�A@{A=�A:��A7�A4A�A2�A01A/?}A.��A-�A*��A*M�A)\)A'ƨA'�A&�jA%VA$��A$^5A#�^A#7LA"��A"^5A"  A!\)A ��A JA33A�uA5?A��A�;A�wA��A�yA��A�!A�A�Ax�A{AAI�A9XA�uA�RAZAQ�A��A
�+A	C�A	�A��A33A`BA\)A�AM�A��A�/A�At�A�A ��A ~�A��A��A�-A��A�wAV@�I�@�x�A �@��-@���@��H@�o@�r�@�K�@�V@�j@�\)@��T@�X@�h@�bN@�+@��@���@���@�@��m@�/@ҏ\@�@ѡ�@ύP@�x�@���@̓u@���@���@š�@°!@�r�@��@�~�@�{@���@��y@þw@�V@�X@Ə\@�o@�b@ɺ^@��#@�O�@��@ɉ7@ɩ�@ɉ7@��@ǶF@��@ư!@�5?@��#@�G�@��@�(�@���@�S�@�hs@�r�@�9X@�Q�@�Q�@�1@��@��@�@��@��@��@�S�@�o@�ff@���@��h@��@��@���@��P@��@��P@��P@��@�"�@���@�$�@�x�@��@���@��@��@��j@�r�@�9X@��;@�33@���@�=q@���@���@�G�@�%@��`@��@��@�bN@�1'@��m@��w@��P@�l�@�ȴ@�v�@�ff@��T@�7L@��/@���@���@��@���@�t�@�S�@�33@�o@�@���@�v�@�^5@�M�@�-@�{@���@�G�@���@�z�@�Z@� �@�1@�  @���@��;@��
@�S�@��\@��@�hs@�&�@���@���@��D@�I�@��m@��@�o@��y@���@�-@�@��7@�X@�?}@���@���@��9@�z�@�I�@�1@�  @��m@��F@�l�@�C�@��R@�$�@��^@��h@�x�@�/@��@�Ĝ@��@�z�@�Q�@��;@��@��P@�|�@�|�@�;d@��@�l�@�
=@���@�bN@�1@��;@���@�l�@�K�@�+@�@�ȴ@���@�v�@�v�@�v�@�ff@���@�G�@�&�@�%@��@��`@���@��j@���@�I�@��@�K�@���@�V@�=q@��#@��@��@��/@�Ĝ@��u@�A�@��@��F@�\)@���@��!@���@��!@��!@��R@��+@�=q@�$�@��@�`B@���@���@��D@�Q�@���@��F@�S�@��@���@�E�@�-@�{@���@���@�O�@���@���@�I�@��@�t�@�o@��R@���@�~�@��K@x��@htT1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�VB
�bB
�{B
��B
��B
�B
B
ɺB
��B
�B
�sB
�B
��BB�B9XBD�BL�BVB<jB$�BPB
�mB
�HB
�B  BBB�B,B<jBXB`BBcTB`BB^5BW
B\)BXB{�B�JB�^B�yAե�BuB��B��B  B1B�B'�B&�B�B%B�B�mB��B�9B�-B�B��B�uB�B{�Bw�Bq�BiyBZBC�B5?B.B�B
��B
��B
�dB
�B
��B
�hB
{�B
jB
T�B
B�B
49B
�B
uB
+B	��B	�`B	�#B	��B	��B	ƨB	�XB	�!B	��B	�uB	�1B	{�B	ffB	P�B	D�B	=qB	49B	0!B	-B	%�B	�B	�B	oB	B��B�B�B�NB�BǮB�^B�-B�B��B��B��B��B��B��B��B�{B�hB�bB�PB�JB�DB�=B�=B�JB�JB�JB�DB�=B�%B�+B�=B�7B�1B�Bv�Bw�B�B�Bz�By�Bw�B}�Bp�B^5BXB`BBn�Bu�Bt�BaHBM�BI�BH�BN�BP�BF�BM�BO�BJ�BL�BYBo�BcTBbNBdZBZB\)Bn�B�B�VB��B��B��B{�B�+B��B��B�uBt�BjBhsBgmBcTB`BBcTBcTBgmBq�Bs�Bo�BjBe`BbNBaHB`BBaHBcTBffBm�BhsBiyBhsBgmBaHB^5BZB]/B`BBe`BgmBk�Bm�Bx�B��B��B��B�-B�dBŢB��B�B�B�B�NB�sB�B�B��B��B��B	  B	B	B	B		7B	
=B	DB	DB	
=B	JB	PB	VB	bB	\B	\B	bB	�B	�B	�B	�B	�B	$�B	'�B	(�B	(�B	,B	33B	6FB	8RB	9XB	9XB	:^B	;dB	=qB	@�B	D�B	F�B	F�B	F�B	F�B	J�B	N�B	N�B	P�B	R�B	T�B	W
B	[#B	^5B	`BB	bNB	cTB	e`B	gmB	jB	m�B	o�B	p�B	p�B	q�B	u�B	w�B	x�B	{�B	� B	�B	�B	�B	�B	�7B	�JB	�PB	�VB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�3B	�9B	�FB	�RB	�XB	�dB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�BB	�mB	�B	�mB	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
1B
	7B

=B
JB
JB
PB
BB
!bB
,�1111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�JB
�JB
�VB
�bB
�{B
��B
��B
�B
B
ɺB
��B
�B
�sB
�B
��BB�B9XBD�BL�BVB<jB$�BPB
�mB
�HB
�B  BBB�B,B<jBXB`BBcTB`BB^5BW
B\)BXB{�B�JB�^B�yAե�BuB��B��B  B1B�B'�B&�B�B%B�B�mB��B�9B�-B�B��B�uB�B{�Bw�Bq�BiyBZBC�B5?B.B�B
��B
��B
�dB
�B
��B
�hB
{�B
jB
T�B
B�B
49B
�B
uB
+B	��B	�`B	�#B	��B	��B	ƨB	�XB	�!B	��B	�uB	�1B	{�B	ffB	P�B	D�B	=qB	49B	0!B	-B	%�B	�B	�B	oB	B��B�B�B�NB�BǮB�^B�-B�B��B��B��B��B��B��B��B�{B�hB�bB�PB�JB�DB�=B�=B�JB�JB�JB�DB�=B�%B�+B�=B�7B�1B�Bv�Bw�B�B�Bz�By�Bw�B}�Bp�B^5BXB`BBn�Bu�Bt�BaHBM�BI�BH�BN�BP�BF�BM�BO�BJ�BL�BYBo�BcTBbNBdZBZB\)Bn�B�B�VB��B��B��B{�B�+B��B��B�uBt�BjBhsBgmBcTB`BBcTBcTBgmBq�Bs�Bo�BjBe`BbNBaHB`BBaHBcTBffBm�BhsBiyBhsBgmBaHB^5BZB]/B`BBe`BgmBk�Bm�Bx�B��B��B��B�-B�dBŢB��B�B�B�B�NB�sB�B�B��B��B��B	  B	B	B	B		7B	
=B	DB	DB	
=B	JB	PB	VB	bB	\B	\B	bB	�B	�B	�B	�B	�B	$�B	'�B	(�B	(�B	,B	33B	6FB	8RB	9XB	9XB	:^B	;dB	=qB	@�B	D�B	F�B	F�B	F�B	F�B	J�B	N�B	N�B	P�B	R�B	T�B	W
B	[#B	^5B	`BB	bNB	cTB	e`B	gmB	jB	m�B	o�B	p�B	p�B	q�B	u�B	w�B	x�B	{�B	� B	�B	�B	�B	�B	�7B	�JB	�PB	�VB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�!B	�-B	�3B	�9B	�FB	�RB	�XB	�dB	�qB	�wB	�}B	�}B	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�)B	�BB	�mB	�B	�mB	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
%B
1B
	7B

=B
JB
JB
PB
BB
!bB
,�1111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140815                              AO  ARCAADJP                                                                    20181024140815    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140815  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140815  QCF$                G�O�G�O�G�O�0               