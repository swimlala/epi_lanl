CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:23Z AOML 3.0 creation; 2016-08-07T21:36:28Z UW 3.1 conversion     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221323  20160807143628  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_005                   2C  D   APEX                            6531                            072314                          846 @���? 1   @��ww_�@1-O�;dZ�c��Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dyy�D���D�Y�D�l�D���D�  D�I�D��fD�� D�  D�C3D�i�D�ɚD�3D�I�DچfD��D� D�I�D��D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��A ��A ��A@��A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBx=qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C \C\C\C\C\C
\C\C\C\C\C\C\C\C\C\C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8\C:\C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CX\CZ\C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�{C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/�=D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK
=DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��DtФDy}qD���D�[�D�n�D�ιD��D�K�D��RD���D�!�D�ED�k�D�ˆD�D�K�DڈRDྐྵD��D�K�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AܸRAܸRAܾwA���A�A���AܾwA���A���A���A���A�A�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A�ȴA���A���A���A�ȴA�ȴA�ƨA���A���A���Aܧ�Aܗ�Aܡ�Aܣ�A�x�A�;dA�VA�jA�Q�A���A���A��AԬA�n�A�dZA��A���A���A���A�G�A�VA���A�AÇ+A��A¶FA�+A��A��wA�&�A���A��HA�5?A���A�A�A�bNA���A��A��A���A��A��TA��PA��
A�A�A��A�VA�`BA��uA��uA���A��yA�7LA�E�A�r�A���A��DA���A���A�C�A�p�A�
=A�A���A��+A�^5A��A�33A��RA��A�7LA�G�A��yA�&�A��A}�7A{��Az1Ax�+Aw;dAt~�As�Ap�RAl�+Ah��AdĜA_7LAZ�yAW�;AW`BAW?}AV��AT�+AR5?AOx�ALz�AJ�+AH{AE\)AB��A@��A?�A:��A7�FA4�\A21'A/
=A-�A,ĜA,A*(�A'&�A$��A#�
A"��A!�-A��A�jA�#A�!Az�A?}A��A7LA��AXA��A
=A �A��A��A�mA��A�FA�hA+A��A9XAAƨAdZA/A�A�A`BA��A1'AG�A
=A��AQ�AS�A	p�AȴA1A$�A�A��A
AVA
��A
�A	��A�A?}AjA�!A�;AC�A��A (�@�dZ@�
=@��+@�V@�l�@�^5@���@��@�$�@���@�E�@��T@�E�@���@��9@�C�@�33@�|�@�-@�bN@@웦@���@�A�@�5?@��@�I�@��@��m@ް!@�@���@��@�|�@�K�@ڏ\@�hs@��;@֟�@�=q@�X@�V@��/@�z�@�  @Ӆ@�\)@�l�@�l�@�K�@�K�@���@�^5@���@ѡ�@ѡ�@ёh@с@�&�@��m@�\)@�@��@Η�@�$�@͡�@�O�@�bN@�(�@��@���@�dZ@�;d@��@�O�@��@ȣ�@�+@Ɵ�@�M�@�7L@��@�1@°!@��h@��7@�&�@�5?@���@�ȴ@���@���@�n�@��@�9X@��;@��P@�S�@��@��@���@�^5@��@��h@�7L@�7L@���@��m@��@�S�@��^@���@��u@�I�@�1@���@���@��y@���@���@��+@�~�@�$�@�hs@�G�@��@��9@�A�@�S�@�;d@�;d@��H@�-@��#@���@��h@���@�-@�ff@�ff@�=q@�{@���@��^@�V@�Q�@��@��@��@�ff@��^@��@��@���@��F@�t�@�dZ@�ȴ@�n�@��#@�hs@�%@���@��`@��@���@���@�b@���@�\)@�\)@��@���@�5?@��@�J@��@���@�7L@��@�I�@��@���@���@�|�@�dZ@�+@�@��R@�J@��T@���@�&�@��@���@���@�r�@��@�t�@�C�@�o@��y@��+@�=q@�J@��T@���@�?}@�%@��j@��D@�A�@�  @���@���@�l�@�K�@�C�@�33@��@��@��!@�^5@��@���@��h@�O�@�/@�&�@��@�%@��/@��9@��D@�z�@�9X@���@���@�K�@�"�@��y@��!@��\@�J@���@�&�@��j@�r�@�Z@�(�@���@�C�@���@���@�~�@�5?@���@�`B@�&�@���@�I�@�9X@�A�@�(�@��@���@���@�\)@�33@���@��@��+@���@��T@���@���@��@�G�@��@�Ĝ@�9X@� �@��F@�o@���@��9@���@;d@up�@ko@a��@Y��@P��@I��@A%@:-@333@+C�@&E�@"J@5?@��@��@��@
��@�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AܸRAܸRAܾwA���A�A���AܾwA���A���A���A���A�A�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A�ȴA���A���A���A�ȴA�ȴA�ƨA���A���A���Aܧ�Aܗ�Aܡ�Aܣ�A�x�A�;dA�VA�jA�Q�A���A���A��AԬA�n�A�dZA��A���A���A���A�G�A�VA���A�AÇ+A��A¶FA�+A��A��wA�&�A���A��HA�5?A���A�A�A�bNA���A��A��A���A��A��TA��PA��
A�A�A��A�VA�`BA��uA��uA���A��yA�7LA�E�A�r�A���A��DA���A���A�C�A�p�A�
=A�A���A��+A�^5A��A�33A��RA��A�7LA�G�A��yA�&�A��A}�7A{��Az1Ax�+Aw;dAt~�As�Ap�RAl�+Ah��AdĜA_7LAZ�yAW�;AW`BAW?}AV��AT�+AR5?AOx�ALz�AJ�+AH{AE\)AB��A@��A?�A:��A7�FA4�\A21'A/
=A-�A,ĜA,A*(�A'&�A$��A#�
A"��A!�-A��A�jA�#A�!Az�A?}A��A7LA��AXA��A
=A �A��A��A�mA��A�FA�hA+A��A9XAAƨAdZA/A�A�A`BA��A1'AG�A
=A��AQ�AS�A	p�AȴA1A$�A�A��A
AVA
��A
�A	��A�A?}AjA�!A�;AC�A��A (�@�dZ@�
=@��+@�V@�l�@�^5@���@��@�$�@���@�E�@��T@�E�@���@��9@�C�@�33@�|�@�-@�bN@@웦@���@�A�@�5?@��@�I�@��@��m@ް!@�@���@��@�|�@�K�@ڏ\@�hs@��;@֟�@�=q@�X@�V@��/@�z�@�  @Ӆ@�\)@�l�@�l�@�K�@�K�@���@�^5@���@ѡ�@ѡ�@ёh@с@�&�@��m@�\)@�@��@Η�@�$�@͡�@�O�@�bN@�(�@��@���@�dZ@�;d@��@�O�@��@ȣ�@�+@Ɵ�@�M�@�7L@��@�1@°!@��h@��7@�&�@�5?@���@�ȴ@���@���@�n�@��@�9X@��;@��P@�S�@��@��@���@�^5@��@��h@�7L@�7L@���@��m@��@�S�@��^@���@��u@�I�@�1@���@���@��y@���@���@��+@�~�@�$�@�hs@�G�@��@��9@�A�@�S�@�;d@�;d@��H@�-@��#@���@��h@���@�-@�ff@�ff@�=q@�{@���@��^@�V@�Q�@��@��@��@�ff@��^@��@��@���@��F@�t�@�dZ@�ȴ@�n�@��#@�hs@�%@���@��`@��@���@���@�b@���@�\)@�\)@��@���@�5?@��@�J@��@���@�7L@��@�I�@��@���@���@�|�@�dZ@�+@�@��R@�J@��T@���@�&�@��@���@���@�r�@��@�t�@�C�@�o@��y@��+@�=q@�J@��T@���@�?}@�%@��j@��D@�A�@�  @���@���@�l�@�K�@�C�@�33@��@��@��!@�^5@��@���@��h@�O�@�/@�&�@��@�%@��/@��9@��D@�z�@�9X@���@���@�K�@�"�@��y@��!@��\@�J@���@�&�@��j@�r�@�Z@�(�@���@�C�@���@���@�~�@�5?@���@�`B@�&�@���@�I�@�9X@�A�@�(�@��@���@���@�\)@�33@���@��@��+@���@��T@���@���@��@�G�@��@�Ĝ@�9X@� �@��F@�oG�O�@��9@���@;d@up�@ko@a��@Y��@P��@I��@A%@:-@333@+C�@&E�@"J@5?@��@��@��@
��@�R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B
=B	7B	7B	7B1B+BB
��B
�B
�B
�mB
�NB
�BB
�5B
�;B
�;B
�mB
�BB"�B(�B:^BL�B�B��B�'B�FB�NBbB�B1'B0!B1'B��B��BJB��B�B�B�B�fB�/B�B��B��BȴBŢB��B�^B�dB�^B��B��BDB�sBȴB�}B�^B��BB�B��B�XB�Bk�B_;BS�B0!B{B�B
�HB
B
�hB
XB
'�B
!�B
{B
B	��B	�B	�B	�HB	��B	�jB	��B	�uB	|�B	aHB	I�B	=qB	:^B	8RB	49B	'�B	�B	DB��B�B�B�NB�B��BǮB�jB�-B�B��B��B��B��B��B��B��B��B��B��B��B�PB�DB�7B�1B�+B�DB�bB�oB�hB�{B��B�bB�=B�VB�oB��B��B�B�dB�dB�XB�dB�wB��BŢBǮB��B��B�/B�B�B	{B	!�B	"�B	�B	B�B�B�B	  B	bB	�B	+B	@�B	C�B	B�B	A�B	;dB	8RB	49B	+B	$�B	 �B	�B	uB	hB	bB	PB		7B	B	1B	JB	)�B	9XB	5?B	/B	/B	1'B	0!B	%�B	!�B	#�B	,B	+B	&�B	!�B	�B	PB	%B		7B		7B	VB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	&�B	,B	.B	0!B	1'B	1'B	2-B	33B	7LB	>wB	?}B	B�B	B�B	B�B	C�B	B�B	C�B	C�B	C�B	E�B	F�B	F�B	L�B	P�B	R�B	R�B	R�B	R�B	R�B	W
B	VB	ZB	]/B	aHB	e`B	e`B	ffB	e`B	l�B	l�B	k�B	o�B	s�B	t�B	{�B	�B	�B	�%B	�+B	�+B	�B	�=B	�=B	�=B	�DB	�PB	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�-B	�3B	�9B	�LB	�^B	�qB	�wB	�wB	�}B	��B	��B	B	ÖB	ÖB	B	B	B	B	B	B	ÖB	ŢB	ƨB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
+B
1B
1B
1B
	7B

=B
	7B
	7B
\B
PB
�B
�B
�B
"�B
-B
1'B
7LB
=qB
E�B
M�B
R�B
YB
]/B
bNB
e`B
iyB
n�B
s�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
:B
:B
7B
9B
9B
9B
9B
;B
9B
;B
;B
9B
9B
;B
9B
9B
7B
7B
;B
7B
9B
9B
9B
9B
9B
;B
;B
9B
9B
;B
9B
;B
;B	4B	5B	5B0B'BB
��B
�B
�}B
�lB
�KB
�@B
�2B
�:B
�;B
�lB
�BB"�B(�B:YBL�B�B��B�B�@B�HB^B�B1 B0B1"B��B��BEB��B�B�B�B�aB�*B�B��B��BȱBŝB�|B�YB�`B�\B��B��B>B�kBȰB�vB�ZB��BB�B�~B�QB�Bk�B_8BS�B0BzB�B
�GB
B
�eB
XB
'�B
!�B
|B
B	��B	�B	�B	�MB	��B	�mB	��B	�zB	|�B	aPB	I�B	=zB	:gB	8\B	4?B	'�B	�B	NB��B��B�B�YB�B��BǺB�wB�;B�B��B��B��B��B��B��B��B��B��B��B��B�`B�RB�FB�=B�<B�SB�oB�|B�wB��B��B�pB�LB�bB�}B��B��B�%B�oB�qB�fB�qB��B��BūBǵB��B��B�9B�B�B	�B	!�B	"�B	�B	(B�B�B�B	 	B	jB	�B	+B	@�B	C�B	B�B	A�B	;kB	8XB	4AB	+	B	$�B	 �B	�B	|B	rB	jB	YB		?B	&B	:B	RB	*B	9[B	5GB	/!B	/#B	1/B	0)B	%�B	!�B	#�B	,B	+	B	&�B	!�B	�B	XB	+B		AB		?B	]B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	&�B	,B	.B	0(B	1-B	1-B	21B	39B	7QB	>|B	?�B	B�B	B�B	B�B	C�B	B�B	C�B	C�B	C�B	E�B	F�B	F�B	L�B	P�B	R�B	R�B	R�B	R�B	R�B	WB	V	B	Z#B	]1B	aIB	eeB	ecB	fkB	ebB	l�B	l�B	k�B	o�B	s�B	t�B	{�B	�B	�B	�)B	�+B	�-B	�"B	�>B	�@B	�AB	�EB	�UB	�eB	�lB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	��B	��B	�B	�B	�B	�B	�B	�(B	�-B	�.B	�6B	�9B	�MB	�^B	�qB	�xB	�yB	�~B	��B	��B	B	×B	ÖB	B	B	B	B	B	B	ÕB	šB	ƨB	ǯB	ǮB	ǰB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�(B	�(B	�(B	�,B	�0B	�.B	�.B	�/B	�6B	�4B	�8B	�BB	�?B	�HB	�FB	�MB	�MB	�LB	�KB	�SB	�XB	�_B	�fB	�gB	�lB	�oB	�tB	�tB	�sB	�sB	�zB	�xB	�zB	�}B	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B

B
B
B
B
B
B
B
B
#B
*B
'B
+B
1B
,B
2B
2B
1B
	3B

>B
	4B
	3G�O�B
LB
�B
�B
�B
"�B
-B
1$B
7IB
=mB
E�B
M�B
R�B
YB
](B
bLB
e]B
itB
n�B
s�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.06 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436282016080714362820160807143628  AO  ARCAADJP                                                                    20150226221323    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221323  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221323  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143628  IP                  G�O�G�O�G�O�                