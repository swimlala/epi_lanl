CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-04-14T11:21:24Z creation; 2022-09-06T18:14:19Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  cd   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 2�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � :4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � X�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � `�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` @   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210414112124  20220906181419  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               n   nAA  AOAO7824_008764_110                 7824_008764_110                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�mF�k�@�mF�k�11  @�mxF�@�mxF�@7����o@7����o�d�M3H,�d�M3H,11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @@  @}p�@��R@�  @�G�A   A��A ��A,��A@��A`��A�Q�A�  A��A�  A�Q�AϮA�  A�  B   B  B  B  B�
B'�
B/�
B8  B@(�BH(�BP  BX  B`(�Bh(�Bp  Bw�
B�B��B��B�  B�  B�{B�  B��B��B��B��B�  B�{B��B��B��
B�  B�{B�{B�(�B�{B�  B�  B�  B�  B�{B�{B��B��B��B�  B�{C 
=C��C�C��C  C
  C  C
=C  C  C
=C
=C
=C
=C  C  C 
=C"
=C$  C&  C(  C*  C,
=C.
=C0  C1��C3��C6  C8
=C:  C<  C>  C@
=CB  CC��CE��CG��CJ  CL
=CN
=CP
=CR
=CT  CU��CW��CY��C\  C^  C`  Cb  Cd  Ce��Cg��Cj  Cl  Cm��Co��Cr  Ct
=Cv
=Cx
=Cz  C{��C~  C�C�  C���C�  C�C���C�  C���C�  C�C���C���C�  C�C�  C�  C���C�  C�C�C�C�C�  C���C���C�  C�  C���C���C���C�C�  C���C�  C�C�
=C�  C���C�  C�  C�  C�  C�  C�C���C�  C�C�C�C�C�
=C�  C���C���C���C���C�  C�  C���C���C���C���C���C�  C�C�C�C�C���C���C���C���C���C�  C�  C�  C�  C�  C�  C�C�C�C�  C�  C�
=C�C�  C�  C�  C���C�  C�  C���C���C�  C�  C�C���C�  C�  C�C�C�  C�  C�  C���C���C���C���C���C���C�  C�C�  C���C���C���C���C���C���C�  C�C�C�  C���C�  C�  C���D �D ��D �qD� D�D}qD��D� D�D� D�qD��D�D� D  Dz�D  D� D	  D	�D
D
��D  D��D  D}qD�D� D  D� D  D� D�qD� D�D�D�D� D  D� D  D� D�D}qD  D� D  D}qD�qD� D  D� D  D� D  D}qD�qD� D  D� D  D}qD�qD� D�qD }qD!  D!��D"D"� D"�qD#� D$�D$�D%�D%��D&  D&� D'�D'��D(�D(� D)  D)��D*�D*� D+�D+� D,  D,� D-  D-� D.  D.� D/�D/��D0  D0z�D0�qD1� D2  D2� D3  D3}qD4  D4��D5�D5� D6  D6}qD6�qD7��D8  D8}qD8�qD9}qD:  D:� D;  D;� D<�D<��D=�D=� D=�qD>}qD?  D?}qD?�qD@� DA�DA� DB  DB}qDB�qDC}qDD  DD� DE  DE}qDF  DF� DF�qDGz�DG�qDH��DI�DI��DJ  DJ}qDJ�qDK� DL  DL� DM  DM� DN  DN� DO�DO}qDP  DP� DQ  DQ� DR  DR��DS�DS�DT�DT� DU�DU��DV  DV� DW�DW� DW�qDX}qDX�qDYz�DZ  DZ�D[  D[z�D\�D\��D\�qD]}qD]�qD^}qD^�qD_� D`  D`��D`�qDa� DbDb� Db�qDc}qDd  Dd}qDd��De}qDf  Df� Df�qDg� Dh  Dh}qDi�Di��Dj�Dj�Dk�Dk��Dl  Dlz�Dl�qDm� Dn�Dn�Do�Do}qDp  Dp��Dq  Dqz�Dr  Dr��Ds  Ds� Dt  Dt� Du  Du}qDv  Dv� Dw�Dw��Dx  Dx��Dy  Dy� Dz  Dz� D{  D{� D|  D|}qD|�qD}� D~�D~��D�D� D�  D�>�D�~�D���D�  D�AHD�� D�� D���D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�HD�>�D�� D��HD�  D�AHD��HD�� D���D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�@ D�� D���D���D�AHD��HD��HD���D�>�D�� D�� D�  D�>�D�~�D���D��qD�@ D��HD�� D�  D�>�D�� D��HD�HD�@ D��HD��HD�HD�AHD�� D���D���D�>�D�� D���D�  D�AHD��HD�� D���D�>�D�� D�D�  D�@ D�~�D�� D�HD�AHD�� D���D���D�@ D�~�D�� D�HD�@ D�� D��HD�HD�AHD�� D�� D�HD�@ D�~�D��qD��qD�=qD�~�D�� D�HD�AHD�� D���D���D�>�D�~�D���D�  D�@ D�� D���D���D�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D�HD�>�D�� D��HD�  D�>�D�~�D���D���D�>�D�~�D���D���D�@ D�� D��HD�HD�@ D�}qD���D�  D�AHD��HD�� D���D�AHD�� D�� D�  D�>�D�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�>�D�� D��HD�  D�=qD�~�D�� D�  D�>�D�}qD�� D�HD�@ D�� D���D��qD�=qD�~�D��qD���D�@ D��HD�D�HD�>�D�� D��HD��D�B�D���D�� D�  D�AHD�� D���D�HD�AHD�� D�� D�HD�AHD�� D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D��qD�>�D��HD��HD�  D�@ D��HD���D�  D�@ D�~�D���D���D�@ D��HD�� D�HD�AHD�~�D�� D�  D�@ D��HD�� D�  D�AHD�� D�� D�  D�@ D D�� D�  D�>�D�~�D�� D�  D�AHDĀ D�� D�  D�>�Dŀ D��HD�  D�>�Dƀ D�� D���D�@ DǁHD��HD���D�@ DȀ D�� D�  D�@ D�~�Dɾ�D���D�@ DʁHD�� D�  D�@ Dˀ D�� D�  D�AHD̀ D̾�D���D�@ D�~�D;�D���D�@ D΀ D�� D�  D�@ D�~�D�� D�  D�@ DЀ Dо�D���D�>�Dр D�� D�HD�AHDҀ D��HD�HD�AHDӀ D��HD�HD�AHDԀ D�� D�  D�>�DՁHD��HD�  D�@ Dր D־�D��qD�>�DׁHD��HD�  D�>�D�~�D�� D�  D�AHDفHD�� D���D�@ Dڀ D�� D�  D�AHDہHD��HD�HD�@ D�~�D�� D�HD�@ D݀ D�� D���D�>�D�}qD޾�D�  D�>�D�~�D߽qD�  D�B�D���D�� D���D�@ D� D�� D���D�@ D� D�� D���D�>�D� D��HD�  D�@ D� D�� D���D�@ D�HD�� D���D�>�D�~�D澸D���D�>�D�~�D�� D�  D�@ D� D辸D���D�>�D�~�D�� D�  D�@ D� D꾸D���D�>�D� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D��HD�  D�@ D�HD�� D���D�AHD� DﾸD�HD�B�D�� D�D���D�>�D�~�D�� D�  D�AHD�D�� D���D�@ D� D�D��)>�G�?\)?W
=?���?��?�@��@(��@8Q�@O\)@c�
@z�H@�ff@���@��H@�ff@��@���@��
@�{@�Q�@��@�{@�Q�AG�A
=A�A�A�A��A!�A&ffA-p�A2�\A7
=A<��AB�\AG
=AL��AQG�AW�A\��Aa�Ag
=Al(�AqG�AvffAz�HA~�RA��A��A�p�A��A�G�A�33A���A��RA�Q�A��A��
A�A�
=A���A��HA���A�ffA���A��\A�z�A�ffA�Q�A��\A�(�A�ffA���A��\A���A��RA���A��HA���A�\)A�G�AÅA�p�A�\)Aə�A˅A�{AϮA��A��
A�{A�  A�=qA�z�A�{A�Q�A�\A�z�A�RA��A��HA��A�
=A���A�33A�p�A��A��A��
A�A��B ��B�B�HB�
B��BB�HB�
B��B	B
�RB�B��B��B�\B�Bz�BG�BffB\)BQ�Bp�B=qB\)BQ�B�B=qB
=B(�BG�B{B33B (�B!G�B"{B#
=B$(�B%�B&{B'33B((�B)�B*=qB+33B,Q�B-G�B.ffB/\)B0Q�B1p�B2�\B3�B4��B5��B6�RB7�
B8��B9�B:�HB<  B=�B>=qB?33B@(�BAG�BB=qBC\)BDz�BE��BFffBG�BH��BIBJ�RBK�
BL��BM�BN�HBP(�BQ�BR=qBS\)BTQ�BUp�BV�\BW\)BXz�BY��BZ�RB[�B\��B]B^�HB_�
B`��Ba�Bc33Bd  Be�Bf{Bg
=BhQ�BiG�Bj=qBk33BlQ�BmG�Bn=qBo�Bpz�Bq��Br�\Bs�Bt��Bu��Bv�HBw�Bx��By�Bz�HB|(�B|��B~{B33B�(�B���B�G�B�B�Q�B���B�\)B�  B�z�B���B��B�{B���B��B���B�=qB��RB�\)B��
B�ffB��HB�p�B��B��\B�
=B���B�{B���B�G�B�B�=qB��HB�\)B��B�z�B�
=B��B�{B��RB�33B��B�(�B��RB�G�B��
B�Q�B���B��B�  B��\B�
=B���B�=qB��RB�G�B��
B�ffB���B��B�{B���B�33B��B�=qB��HB�\)B��B�z�B�
=B���B�{B��RB�G�B�B�Q�B��HB�p�B�{B��\B��B��B�Q�B���B�\)B��B��\B�
=B��B�=qB��RB�G�B��
B�ffB���B��B�{B��\B�33B�B�ffB���B�p�B�  B��\B��B�B�ffB��HB�p�B�{B��\B��B�B�=qB��HB�p�B�  B��\B��B��B�Q�B���B�G�B��Bď\B��BŮB�=qB��HB�\)B��Bȏ\B��BɮB�Q�B��HB�p�B�  B̏\B��BͮB�=qB��HB�\)B�  BЏ\B�
=BѮB�=qB���B�\)B��B�z�B��B�B�Q�B��HB�p�B�  B؏\B��BٮB�=qB��HB�p�B�  Bܣ�B�33B�B�ffB���B߅B�{B�RB�G�B��
B�z�B�
=B㙚B�=qB���B�\)B�  B�\B��B�B�Q�B���B�B�{B�RB�G�B��B�z�B��B�B�ffB���BB�(�B�RB�\)B�  B��B�33B�B�z�B�
=B���B�=qB��HB�\)B�  B���B�33B�B�z�B��B��B�=qB��HB�p�B�  B���B�G�B�C 33C z�C C{CffC�C��C=qC�C�HC(�Cp�CC{C\)C�C  CQ�C��C��C=qC�\C�HC=qC�C�
C33Cz�C��C	�C	p�C	C
{C
ffC
�RC  C\)C�C
=CQ�C��C��CQ�C��C��CG�C��C�C=qC�\C�C=qC�\C�HC33C�C�
C(�Cz�C�
C�Cp�CC�Cp�CC
=CffC�RC  C\)C�C��CG�C�\C�HC(�Cz�C��C�CffC�C��C(�Cz�C�C�HC�CQ�Cz�C�C�HC
=C33C\)Cz�C�RC�HC{C=qCffC�\C�RC�C{C=qCffC��C��C��C(�CQ�Cz�C�C�
C 
=C 33C ffC �\C C �C!{C!G�C!p�C!��C!��C!��C"(�C"\)C"�C"�RC"�
C#{C#=qC#ffC#�\C#C#��C$�C$G�C$p�C$��C$��C%  C%�C%Q�C%�C%��C%�
C&
=C&=qC&\)C&�C&�RC&�HC'
=C'=qC'ffC'��C'C'��C((�C(G�C(�C(��C(�
C)  C)33C)\)C)�\C)C)�C*{C*G�C*p�C*��C*�
C+  C+33C+\)C+�C+�RC+�C,{C,G�C,ffC,��C,��C,��C-(�C-Q�C-�C-��C-�
C.  C.33C.\)C.�\C.C.�HC/{C/=qC/p�C/��C/C0  C0(�C0Q�C0�C0�C0�HC1
=C133C1ffC1��C1�RC1��C2�C2G�C2z�C2��C2�
C3
=C3(�C3\)C3�\C3�RC3�HC4{C4G�C4z�C4��C4�
C5
=C533C5ffC5�\C5��C6  C633C6\)C6�\C6C6��C7(�C7\)C7�\C7�RC7��C8(�C8Q�C8z�C8�RC8�HC9{C9G�C9p�C9��C9��C:  C:33C:ffC:�\C:C;  C;(�C;\)C;�\C;C;�C<(�C<Q�C<�C<�RC<�C=�C=G�C=�C=�RC=�C>{C>Q�C>z�C>�RC>�HC?�C?Q�C?�C?C?�C@(�C@\)C@�\C@CA  CA33CA\)CA�\CA�
CB  CB33CBffCB��CB��CC
=CC=qCCp�CC�CC�
CD�CDG�CD�CD�RCD�CE�CE\)CE�\CECE��CF(�CF\)CF��CF�
CG
=CG=qCGp�CG��CG�
CH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                              ?u@   @@  @}p�@��R@�  @�G�A   A��A ��A,��A@��A`��A�Q�A�  A��A�  A�Q�AϮA�  A�  B   B  B  B  B�
B'�
B/�
B8  B@(�BH(�BP  BX  B`(�Bh(�Bp  Bw�
B�B��B��B�  B�  B�{B�  B��B��B��B��B�  B�{B��B��B��
B�  B�{B�{B�(�B�{B�  B�  B�  B�  B�{B�{B��B��B��B�  B�{C 
=C��C�C��C  C
  C  C
=C  C  C
=C
=C
=C
=C  C  C 
=C"
=C$  C&  C(  C*  C,
=C.
=C0  C1��C3��C6  C8
=C:  C<  C>  C@
=CB  CC��CE��CG��CJ  CL
=CN
=CP
=CR
=CT  CU��CW��CY��C\  C^  C`  Cb  Cd  Ce��Cg��Cj  Cl  Cm��Co��Cr  Ct
=Cv
=Cx
=Cz  C{��C~  C�C�  C���C�  C�C���C�  C���C�  C�C���C���C�  C�C�  C�  C���C�  C�C�C�C�C�  C���C���C�  C�  C���C���C���C�C�  C���C�  C�C�
=C�  C���C�  C�  C�  C�  C�  C�C���C�  C�C�C�C�C�
=C�  C���C���C���C���C�  C�  C���C���C���C���C���C�  C�C�C�C�C���C���C���C���C���C�  C�  C�  C�  C�  C�  C�C�C�C�  C�  C�
=C�C�  C�  C�  C���C�  C�  C���C���C�  C�  C�C���C�  C�  C�C�C�  C�  C�  C���C���C���C���C���C���C�  C�C�  C���C���C���C���C���C���C�  C�C�C�  C���C�  C�  C���D �D ��D �qD� D�D}qD��D� D�D� D�qD��D�D� D  Dz�D  D� D	  D	�D
D
��D  D��D  D}qD�D� D  D� D  D� D�qD� D�D�D�D� D  D� D  D� D�D}qD  D� D  D}qD�qD� D  D� D  D� D  D}qD�qD� D  D� D  D}qD�qD� D�qD }qD!  D!��D"D"� D"�qD#� D$�D$�D%�D%��D&  D&� D'�D'��D(�D(� D)  D)��D*�D*� D+�D+� D,  D,� D-  D-� D.  D.� D/�D/��D0  D0z�D0�qD1� D2  D2� D3  D3}qD4  D4��D5�D5� D6  D6}qD6�qD7��D8  D8}qD8�qD9}qD:  D:� D;  D;� D<�D<��D=�D=� D=�qD>}qD?  D?}qD?�qD@� DA�DA� DB  DB}qDB�qDC}qDD  DD� DE  DE}qDF  DF� DF�qDGz�DG�qDH��DI�DI��DJ  DJ}qDJ�qDK� DL  DL� DM  DM� DN  DN� DO�DO}qDP  DP� DQ  DQ� DR  DR��DS�DS�DT�DT� DU�DU��DV  DV� DW�DW� DW�qDX}qDX�qDYz�DZ  DZ�D[  D[z�D\�D\��D\�qD]}qD]�qD^}qD^�qD_� D`  D`��D`�qDa� DbDb� Db�qDc}qDd  Dd}qDd��De}qDf  Df� Df�qDg� Dh  Dh}qDi�Di��Dj�Dj�Dk�Dk��Dl  Dlz�Dl�qDm� Dn�Dn�Do�Do}qDp  Dp��Dq  Dqz�Dr  Dr��Ds  Ds� Dt  Dt� Du  Du}qDv  Dv� Dw�Dw��Dx  Dx��Dy  Dy� Dz  Dz� D{  D{� D|  D|}qD|�qD}� D~�D~��D�D� D�  D�>�D�~�D���D�  D�AHD�� D�� D���D�>�D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�HD�>�D�� D��HD�  D�AHD��HD�� D���D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�~�D�� D�  D�>�D�� D��HD�HD�@ D�� D�� D�  D�@ D�� D���D���D�AHD��HD��HD���D�>�D�� D�� D�  D�>�D�~�D���D��qD�@ D��HD�� D�  D�>�D�� D��HD�HD�@ D��HD��HD�HD�AHD�� D���D���D�>�D�� D���D�  D�AHD��HD�� D���D�>�D�� D�D�  D�@ D�~�D�� D�HD�AHD�� D���D���D�@ D�~�D�� D�HD�@ D�� D��HD�HD�AHD�� D�� D�HD�@ D�~�D��qD��qD�=qD�~�D�� D�HD�AHD�� D���D���D�>�D�~�D���D�  D�@ D�� D���D���D�AHD�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D�HD�>�D�� D��HD�  D�>�D�~�D���D���D�>�D�~�D���D���D�@ D�� D��HD�HD�@ D�}qD���D�  D�AHD��HD�� D���D�AHD�� D�� D�  D�>�D�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D���D�  D�>�D�� D��HD�  D�=qD�~�D�� D�  D�>�D�}qD�� D�HD�@ D�� D���D��qD�=qD�~�D��qD���D�@ D��HD�D�HD�>�D�� D��HD��D�B�D���D�� D�  D�AHD�� D���D�HD�AHD�� D�� D�HD�AHD�� D�� D�HD�@ D�� D��HD�HD�AHD��HD�� D��qD�>�D��HD��HD�  D�@ D��HD���D�  D�@ D�~�D���D���D�@ D��HD�� D�HD�AHD�~�D�� D�  D�@ D��HD�� D�  D�AHD�� D�� D�  D�@ D D�� D�  D�>�D�~�D�� D�  D�AHDĀ D�� D�  D�>�Dŀ D��HD�  D�>�Dƀ D�� D���D�@ DǁHD��HD���D�@ DȀ D�� D�  D�@ D�~�Dɾ�D���D�@ DʁHD�� D�  D�@ Dˀ D�� D�  D�AHD̀ D̾�D���D�@ D�~�D;�D���D�@ D΀ D�� D�  D�@ D�~�D�� D�  D�@ DЀ Dо�D���D�>�Dр D�� D�HD�AHDҀ D��HD�HD�AHDӀ D��HD�HD�AHDԀ D�� D�  D�>�DՁHD��HD�  D�@ Dր D־�D��qD�>�DׁHD��HD�  D�>�D�~�D�� D�  D�AHDفHD�� D���D�@ Dڀ D�� D�  D�AHDہHD��HD�HD�@ D�~�D�� D�HD�@ D݀ D�� D���D�>�D�}qD޾�D�  D�>�D�~�D߽qD�  D�B�D���D�� D���D�@ D� D�� D���D�@ D� D�� D���D�>�D� D��HD�  D�@ D� D�� D���D�@ D�HD�� D���D�>�D�~�D澸D���D�>�D�~�D�� D�  D�@ D� D辸D���D�>�D�~�D�� D�  D�@ D� D꾸D���D�>�D� D�� D�HD�@ D�~�D�� D�HD�@ D�~�D��HD�  D�@ D�HD�� D���D�AHD� DﾸD�HD�B�D�� D�D���D�>�D�~�D�� D�  D�AHD�D�� D���D�@ D� D�G�O�>�G�?\)?W
=?���?��?�@��@(��@8Q�@O\)@c�
@z�H@�ff@���@��H@�ff@��@���@��
@�{@�Q�@��@�{@�Q�AG�A
=A�A�A�A��A!�A&ffA-p�A2�\A7
=A<��AB�\AG
=AL��AQG�AW�A\��Aa�Ag
=Al(�AqG�AvffAz�HA~�RA��A��A�p�A��A�G�A�33A���A��RA�Q�A��A��
A�A�
=A���A��HA���A�ffA���A��\A�z�A�ffA�Q�A��\A�(�A�ffA���A��\A���A��RA���A��HA���A�\)A�G�AÅA�p�A�\)Aə�A˅A�{AϮA��A��
A�{A�  A�=qA�z�A�{A�Q�A�\A�z�A�RA��A��HA��A�
=A���A�33A�p�A��A��A��
A�A��B ��B�B�HB�
B��BB�HB�
B��B	B
�RB�B��B��B�\B�Bz�BG�BffB\)BQ�Bp�B=qB\)BQ�B�B=qB
=B(�BG�B{B33B (�B!G�B"{B#
=B$(�B%�B&{B'33B((�B)�B*=qB+33B,Q�B-G�B.ffB/\)B0Q�B1p�B2�\B3�B4��B5��B6�RB7�
B8��B9�B:�HB<  B=�B>=qB?33B@(�BAG�BB=qBC\)BDz�BE��BFffBG�BH��BIBJ�RBK�
BL��BM�BN�HBP(�BQ�BR=qBS\)BTQ�BUp�BV�\BW\)BXz�BY��BZ�RB[�B\��B]B^�HB_�
B`��Ba�Bc33Bd  Be�Bf{Bg
=BhQ�BiG�Bj=qBk33BlQ�BmG�Bn=qBo�Bpz�Bq��Br�\Bs�Bt��Bu��Bv�HBw�Bx��By�Bz�HB|(�B|��B~{B33B�(�B���B�G�B�B�Q�B���B�\)B�  B�z�B���B��B�{B���B��B���B�=qB��RB�\)B��
B�ffB��HB�p�B��B��\B�
=B���B�{B���B�G�B�B�=qB��HB�\)B��B�z�B�
=B��B�{B��RB�33B��B�(�B��RB�G�B��
B�Q�B���B��B�  B��\B�
=B���B�=qB��RB�G�B��
B�ffB���B��B�{B���B�33B��B�=qB��HB�\)B��B�z�B�
=B���B�{B��RB�G�B�B�Q�B��HB�p�B�{B��\B��B��B�Q�B���B�\)B��B��\B�
=B��B�=qB��RB�G�B��
B�ffB���B��B�{B��\B�33B�B�ffB���B�p�B�  B��\B��B�B�ffB��HB�p�B�{B��\B��B�B�=qB��HB�p�B�  B��\B��B��B�Q�B���B�G�B��Bď\B��BŮB�=qB��HB�\)B��Bȏ\B��BɮB�Q�B��HB�p�B�  B̏\B��BͮB�=qB��HB�\)B�  BЏ\B�
=BѮB�=qB���B�\)B��B�z�B��B�B�Q�B��HB�p�B�  B؏\B��BٮB�=qB��HB�p�B�  Bܣ�B�33B�B�ffB���B߅B�{B�RB�G�B��
B�z�B�
=B㙚B�=qB���B�\)B�  B�\B��B�B�Q�B���B�B�{B�RB�G�B��B�z�B��B�B�ffB���BB�(�B�RB�\)B�  B��B�33B�B�z�B�
=B���B�=qB��HB�\)B�  B���B�33B�B�z�B��B��B�=qB��HB�p�B�  B���B�G�B�C 33C z�C C{CffC�C��C=qC�C�HC(�Cp�CC{C\)C�C  CQ�C��C��C=qC�\C�HC=qC�C�
C33Cz�C��C	�C	p�C	C
{C
ffC
�RC  C\)C�C
=CQ�C��C��CQ�C��C��CG�C��C�C=qC�\C�C=qC�\C�HC33C�C�
C(�Cz�C�
C�Cp�CC�Cp�CC
=CffC�RC  C\)C�C��CG�C�\C�HC(�Cz�C��C�CffC�C��C(�Cz�C�C�HC�CQ�Cz�C�C�HC
=C33C\)Cz�C�RC�HC{C=qCffC�\C�RC�C{C=qCffC��C��C��C(�CQ�Cz�C�C�
C 
=C 33C ffC �\C C �C!{C!G�C!p�C!��C!��C!��C"(�C"\)C"�C"�RC"�
C#{C#=qC#ffC#�\C#C#��C$�C$G�C$p�C$��C$��C%  C%�C%Q�C%�C%��C%�
C&
=C&=qC&\)C&�C&�RC&�HC'
=C'=qC'ffC'��C'C'��C((�C(G�C(�C(��C(�
C)  C)33C)\)C)�\C)C)�C*{C*G�C*p�C*��C*�
C+  C+33C+\)C+�C+�RC+�C,{C,G�C,ffC,��C,��C,��C-(�C-Q�C-�C-��C-�
C.  C.33C.\)C.�\C.C.�HC/{C/=qC/p�C/��C/C0  C0(�C0Q�C0�C0�C0�HC1
=C133C1ffC1��C1�RC1��C2�C2G�C2z�C2��C2�
C3
=C3(�C3\)C3�\C3�RC3�HC4{C4G�C4z�C4��C4�
C5
=C533C5ffC5�\C5��C6  C633C6\)C6�\C6C6��C7(�C7\)C7�\C7�RC7��C8(�C8Q�C8z�C8�RC8�HC9{C9G�C9p�C9��C9��C:  C:33C:ffC:�\C:C;  C;(�C;\)C;�\C;C;�C<(�C<Q�C<�C<�RC<�C=�C=G�C=�C=�RC=�C>{C>Q�C>z�C>�RC>�HC?�C?Q�C?�C?C?�C@(�C@\)C@�\C@CA  CA33CA\)CA�\CA�
CB  CB33CBffCB��CB��CC
=CC=qCCp�CC�CC�
CD�CDG�CD�CD�RCD�CE�CE\)CE�\CECE��CF(�CF\)CF��CF�
CG
=CG=qCGp�CG��CG�
CH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                              @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�|�A�z�A��7A��7A��7A��7A��+A��+A��+A��A��+A��+A��A��A��+A��A��A��A�t�A�`BA�XA�O�A�9XA�A���A�1'A��A��A�/A��A���A���A�v�A�ffA�l�A�p�A�S�A�&�A��TA���A��A�(�A�A��A�ȴA���A�Q�A�$�A���A��;A���A��wA��-A���A���A��hA�z�A�?}A��A���A��FA��\A�ZA�JA��-A�v�A�I�A��/A�;dA��!A���A��
A�v�A�&�A��A�ĜA�;dA�  A��/A���A�VA�(�A�ĜA��A��A�1'A���A��
A��!A�S�A���A��9A��7A��PA�1A�&�A�A��^A�x�A�&�A�XA�`BA��hA�t�A���A���A��A��A�C�A�  A�ZA���A�%A��9A��uA��^A�r�A��;A�(�A�C�A��jA�K�A���A�p�A��^A��wA��A���A�jA�ȴA�O�A�E�A��PA�S�A�=qA��!A�jA��yA��A~ �A|1'Azn�Ax��Aw��Av{Au�^Au|�At�/AsO�Aq��Ao�An�HAmAl��Aj��Ai�Ah�Ah1'Af�Ad�A`�RA[��AZ-AYC�AXAU�AT1'AS\)AR�RAQG�AP�APQ�AOG�AN�/ANv�AM?}AKdZAJr�AI��AI?}AIAH�AF��AE�-AD�HAD(�AC`BAB�/AB5?A@�\A>�jA=|�A<�A;�A:ĜA9��A7&�A7
=A6bNA5ƨA2z�A1�A0E�A.�DA-A,�!A+ƨA+��A+?}A*~�A(��A(ZA(�A'��A&�`A&A$^5A!��A ��At�A�AdZAbNAv�AO�An�A�FA��AM�A�-A`BA~�A��A��A=qA��A�A��A-A/A�A%AVA�A��A��A�AC�A	�
A��AVA�FA��A�TA7LA"�AE�A|�A A�@�^5@�G�@��@���@��#@�V@��;@��@���@���@�!@�@@�~�@��@홚@���@��@�ȴ@�^5@�@�@旍@�M�@�-@�-@���@��m@�@��@�ȴ@ᙚ@��;@�n�@�{@�{@�p�@�Ĝ@�r�@�@�{@�hs@�bN@�K�@�n�@Ցh@Ԭ@ӍP@ҸR@љ�@���@���@Л�@�z�@�(�@�\)@�ȴ@���@̼j@�ƨ@�S�@�C�@���@�~�@�@�A�@Ɵ�@���@�7L@�Z@�I�@�9X@�  @��@�$�@��@�I�@��w@�\)@�K�@���@���@��7@��@�1'@�l�@��+@��-@��@���@�dZ@��H@��+@�J@��`@���@��!@�^5@�5?@��^@���@�5?@���@�-@��7@�?}@���@��D@�b@��m@�\)@�$�@�/@�`B@���@���@��@�G�@�I�@��R@���@��@���@��@���@�
=@���@�@��@�I�@��@��@�ȴ@�&�@��@��9@��D@�r�@�r�@�j@�9X@��@��P@��@�;d@���@���@���@�n�@�n�@�ȴ@�
=@���@�{@���@��j@���@��@�@��y@���@���@��+@�~�@�^5@�M�@�E�@�E�@�=q@�5?@�$�@�J@��#@��@���@�$�@�5?@�{@���@���@��@��@�@��h@�O�@�/@���@�1'@�ƨ@��F@���@�;d@���@�{@��@�p�@�O�@�?}@�V@��j@��@�I�@�;d@�@���@���@�l�@�;d@��@�
=@���@���@��\@�5?@��T@��7@�&�@�V@��@��9@��u@�Q�@��m@��w@�S�@���@��!@��!@�v�@�-@��@�{@�@��T@��#@��#@���@���@���@��h@��@���@���@�9X@��;@�ƨ@��w@���@�+@�@��!@�v�@�M�@�5?@�J@��-@�7L@���@���@��9@�Q�@��m@��w@�\)@�+@�@���@�ff@�=q@��@���@��^@���@��@��@��j@�z�@�1'@�@�P@~�R@~E�@}�@}@}@}�h@}?}@|��@{�
@z��@y��@yhs@x��@xA�@w�@wl�@w
=@v�R@v��@v��@v�+@vff@vff@v5?@u�T@u�-@u�h@uO�@u�@t��@t1@s��@sC�@r��@rn�@rM�@rM�@r�@q�#@qx�@qhs@qhs@q7L@q&�@q%@p��@p�9@pbN@p1'@pb@p  @o�@o��@ol�@n�R@nV@n{@m�@m��@m��@m�@l�@l�@l��@kC�@j^5@i��@i%@h�@hr�@hQ�@h �@hb@hb@hb@g��@f�@fv�@f5?@e�h@d�@c"�@b��@b�!@a��@aX@a&�@a7L@a7L@`�`@`�@` �@_�w@_K�@_�@_�@_�@_�@_
=@^�@^5?@^@]��@]O�@\��@[�@ZM�@Y�@X��@XbN@W�w@W�@WK�@V�y@Vȴ@V$�@UO�@T��@T�@S�
@SS�@R�H@R�\@R=q@Q��@Q�@Q�#@Q�^@Qhs@Q�@P�`@Pr�@P1'@P �@Pb@O�@O��@O��@OK�@N��@N�+@N@M��@M�-@M�h@M�h@M�@M/@MV@L�D@K�F@K�F@K��@K�@KdZ@KS�@J�@J=q@I�#@I��@IX@Hr�@G�w@G\)@F��@F��@FE�@F$�@E��@E�@E`B@EO�@E/@D��@D�D@D9X@D�@C��@C�m@C�F@CS�@B��@B�\@Bn�@BM�@A��@Ax�@Ahs@AX@AG�@@��@@��@@�u@@b@?�;@?\)@?
=@?
=@>��@>�y@>�@>��@>E�@>{@=@=��@=�h@=�h@=�@=`B@<��@<�@<�D@<�D@<9X@;�@;33@:�@:��@:��@:~�@:^5@:�@9��@9�@9�^@9hs@8��@8�u@8�u@8�@8�u@8�@8bN@8 �@8  @7�w@7\)@6�R@6V@65?@6$�@6@6{@6{@6{@6{@5�-@5p�@4��@41@3�@3C�@2�!@2=q@2M�@2=q@1�#@1�7@1X@1%@0�`@0��@0bN@/��@/�P@/l�@/l�@/\)@/K�@/+@/�@/
=@.�y@.ȴ@.5?@-@-�@,��@,�j@,��@,j@+�@+@*�!@*M�@*J@)��@)X@)%@(Ĝ@(r�@( �@'�w@'�P@'|�@'\)@';d@&��@&@%�h@%`B@%�@%V@%V@$�/@$�@$�@$��@$z�@$9X@#�m@#�
@#��@#S�@"^5@"J@!�@!�^@!��@!7L@ �`@ �9@ �@ 1'@   @��@\)@;d@+@+@�@�@
=@�y@�R@E�@@(�@�F@�@S�@C�@33@"�@o@@@��@n�@M�@=q@��@�#@��@�^@�7@hs@7L@��@��@r�@Q�@1'@b@  @�;@�@|�@+@��@��@V@E�@$�@@�@�h@`B@/@/@/@�@�/@�/@�/@��@��@��@�j@�D@9X@�m@t�@C�@33@�H@��@��@�!@��@�\@n�@=q@��@��@��@x�@hs@hs@X@G�@G�@&�@%@�`@��@��@bN@b@��@\)@�@��@�+@v�@ff@ff@E�@@��@@�-@�-@�-@��@��@/@�/@��@�j@�@��A�~�A�z�A�|�A��A�|�A�v�A�z�A��A��7A��7A��7A��7A��DA��7A��A��7A��DA��+A��A��A��7A��7A��+A��A��+A��7A��7A��A��A��+A��+A��+A��A��A��A��7A��DA��+A��A��A��A��+A��7A��+A��A��A��+A��+A��+A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�~�A�~�A��A��A��A��A��A��+A��7A��DA��7A��A��A�~�A��A��A��+A��7A��A�z�A�x�A�z�A�|�A�|�A�|�A�z�A�p�A�jA�l�A�n�A�n�A�jA�bNA�ZA�ZA�ZA�XA�VA�S�A�VA�XA�\)A�ZA�ZA�ZA�VA�S�A�S�A�VA�Q�A�Q�A�M�A�O�A�O�A�E�A�C�A�9XA�7LA�5?A�&�A�"�A�&�A�$�A��A��A�1A���A���A���A���A�A���A���A��A��7A�t�A�l�A�p�A�\)A�O�A�E�A��A�bA�JA�A�1A�A���A���A��A��TA���A���A���A���A�A��RA���A���A��uA�~�A�`BA�K�A�=qA�(�A� �A�bA��A�JA�%A��A��A��A��`A��mA��`A��`A��mA��HA�ȴA�ƨA�ĜA�A��wA��jA��^A��A���A���A���A���A��uA��A��PA��A�hsA�jA�jA�ffA�dZA�dZA�ffA�ffA�dZA�hsA�jA�hsA�hsA�jA�l�A�n�A�n�A�l�A�n�A�p�A�p�A�n�A�p�A�t�A�t�A�p�A�`BA�^5A�\)A�S�A�Q�A�O�A�M�A�I�A�K�A�I�A�?}A�1'A� �A�bA�1A�  A���A��A��`A��/A��A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA��RA��+A�K�A�A�A�;dA�5?A�5?A�33A�-A�&�A�"�A��A�{A�JA�A�A�A�A�A���A�  A�  A���A��A��`A��/A��A��
A���A���A�ȴA�ƨA�A���A���A��RA��A���A��uA���A��hA�~�A�p�A�^5A�M�A�I�A�K�A�C�A�?}A�?}A�?}A�;dA�"�A�{A�bA�bA�JA�%A�  A�  A�A���A��A��A��A��TA��;A��;A��A��A��
A���A���A���A���A���A���A���A���A�ĜA��RA��^A��RA��9A��FA��RA��FA��!A��!A��-A��9A��A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��PA��PA��hA��hA��7A��A��A��A�p�A�r�A�r�A�ffA�ZA�Q�A�G�A�"�A� �A�-A�(�A��A��A��A��A��A��A��A�{A�
=A���A��A��HA���A�ĜA�ĜA��wA��RA��-A���A���A���A���A���A��uA��PA��A�r�A�p�A�l�A�\)A�XA�M�A�G�A�?}A�/A�"�A��A�%A��A��HA���A���A�A���A���A���A��uA��hA��+A�|�A�n�A�dZA�`BA�`BA�^5A�XA�S�A�O�A�33A�+A��A�%A���A��`A���A���A��A�bNA�VA�G�A�9XA�$�A��A�VA���A��#A��!A���A��uA�p�A�C�A�  A��jA���A�x�A�S�A�5?A�%A�1A�ĜA��^A��jA��wA��FA���A���A�v�A�M�A�E�A�G�A�;dA�(�A�&�A� �A� �A��A��A��A�{A��A��A�bA�A��A�ƨA��!A��A���A���A��uA�jA�33A�
=A�  A���A�A�A���A�  A�  A���A���A��A��`A��
A�ĜA��jA��-A���A���A���A��uA��7A�z�A�bNA�ZA�Q�A�O�A�G�A�?}A�?}A�9XA�1'A�+A�oA�%A�A���A��A��;A���A��^A�v�A�I�A�1'A�+A��A�bA���A��A��yA��;A���A��RA���A���A��DA�XA�VA�A���A��A��/A���A�z�A�p�A�ffA�`BA�K�A�G�A�=qA�;dA�/A�(�A��A��A�{A�1A�A�  A���A���A���A���A���A���A��A���A��A��A��A��A��mA��TA��A���A�ȴA�ƨA�ĜA���A�ĜA���A��wA�A��wA��wA��RA��9A���A���A���A��PA��7A��+A�x�A�hsA�`BA�XA�M�A�/A�5?A�/A�+A��A��A��A�bA�A���A��A��mA��yA��mA��mA��yA��;A��
A���A�ĜA��wA���A���A���A���A��hA���A���A���A���A���A���A��uA��hA��A�z�A�l�A�bNA�XA�7LA��A��HA��-A���A�r�A�1'A�oA�bA��`A���A��A��hA�r�A�A�A��A���A��A���A��A�jA�`BA�XA�Q�A�O�A�K�A�9XA�VA�
=A�1A���A���A��!A���A���A�t�A�z�A�+A��A�ȴA�\)A�5?A�(�A�(�A�"�A� �A�bA�
=A��A��A�5?A�$�A�
=A���A���A��yA��yA�ȴA��wA��7A�=qA���A���A���A�\)A�O�A�Q�A�G�A��A�oA�{A�VA��TA��
A��mA�A��+A�r�A�ZA�/A� �A�{A�A��A��-A��uA���A���A�x�A�XA�^5A�+A���A��/A���A���A��FA��9A���A���A�|�A�I�A�-A��A���A���A��A�~�A�C�A��A��A�JA�A��A��HA��FA���A��uA��A�r�A�ffA�I�A��A�%A���A��/A�ƨA��^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                              A�|�A�z�A��7A��7A��7A��7A��+A��+A��+A��A��+A��+A��A��A��+A��A��A��A�t�A�`BA�XA�O�A�9XA�A���A�1'A��A��A�/A��A���A���A�v�A�ffA�l�A�p�A�S�A�&�A��TA���A��A�(�A�A��A�ȴA���A�Q�A�$�A���A��;A���A��wA��-A���A���A��hA�z�A�?}A��A���A��FA��\A�ZA�JA��-A�v�A�I�A��/A�;dA��!A���A��
A�v�A�&�A��A�ĜA�;dA�  A��/A���A�VA�(�A�ĜA��A��A�1'A���A��
A��!A�S�A���A��9A��7A��PA�1A�&�A�A��^A�x�A�&�A�XA�`BA��hA�t�A���A���A��A��A�C�A�  A�ZA���A�%A��9A��uA��^A�r�A��;A�(�A�C�A��jA�K�A���A�p�A��^A��wA��A���A�jA�ȴA�O�A�E�A��PA�S�A�=qA��!A�jA��yA��A~ �A|1'Azn�Ax��Aw��Av{Au�^Au|�At�/AsO�Aq��Ao�An�HAmAl��Aj��Ai�Ah�Ah1'Af�Ad�A`�RA[��AZ-AYC�AXAU�AT1'AS\)AR�RAQG�AP�APQ�AOG�AN�/ANv�AM?}AKdZAJr�AI��AI?}AIAH�AF��AE�-AD�HAD(�AC`BAB�/AB5?A@�\A>�jA=|�A<�A;�A:ĜA9��A7&�A7
=A6bNA5ƨA2z�A1�A0E�A.�DA-A,�!A+ƨA+��A+?}A*~�A(��A(ZA(�A'��A&�`A&A$^5A!��A ��At�A�AdZAbNAv�AO�An�A�FA��AM�A�-A`BA~�A��A��A=qA��A�A��A-A/A�A%AVA�A��A��A�AC�A	�
A��AVA�FA��A�TA7LA"�AE�A|�A A�@�^5@�G�@��@���@��#@�V@��;@��@���@���@�!@�@@�~�@��@홚@���@��@�ȴ@�^5@�@�@旍@�M�@�-@�-@���@��m@�@��@�ȴ@ᙚ@��;@�n�@�{@�{@�p�@�Ĝ@�r�@�@�{@�hs@�bN@�K�@�n�@Ցh@Ԭ@ӍP@ҸR@љ�@���@���@Л�@�z�@�(�@�\)@�ȴ@���@̼j@�ƨ@�S�@�C�@���@�~�@�@�A�@Ɵ�@���@�7L@�Z@�I�@�9X@�  @��@�$�@��@�I�@��w@�\)@�K�@���@���@��7@��@�1'@�l�@��+@��-@��@���@�dZ@��H@��+@�J@��`@���@��!@�^5@�5?@��^@���@�5?@���@�-@��7@�?}@���@��D@�b@��m@�\)@�$�@�/@�`B@���@���@��@�G�@�I�@��R@���@��@���@��@���@�
=@���@�@��@�I�@��@��@�ȴ@�&�@��@��9@��D@�r�@�r�@�j@�9X@��@��P@��@�;d@���@���@���@�n�@�n�@�ȴ@�
=@���@�{@���@��j@���@��@�@��y@���@���@��+@�~�@�^5@�M�@�E�@�E�@�=q@�5?@�$�@�J@��#@��@���@�$�@�5?@�{@���@���@��@��@�@��h@�O�@�/@���@�1'@�ƨ@��F@���@�;d@���@�{@��@�p�@�O�@�?}@�V@��j@��@�I�@�;d@�@���@���@�l�@�;d@��@�
=@���@���@��\@�5?@��T@��7@�&�@�V@��@��9@��u@�Q�@��m@��w@�S�@���@��!@��!@�v�@�-@��@�{@�@��T@��#@��#@���@���@���@��h@��@���@���@�9X@��;@�ƨ@��w@���@�+@�@��!@�v�@�M�@�5?@�J@��-@�7L@���@���@��9@�Q�@��m@��w@�\)@�+@�@���@�ff@�=q@��@���@��^@���@��@��@��j@�z�@�1'@�@�P@~�R@~E�@}�@}@}@}�h@}?}@|��@{�
@z��@y��@yhs@x��@xA�@w�@wl�@w
=@v�R@v��@v��@v�+@vff@vff@v5?@u�T@u�-@u�h@uO�@u�@t��@t1@s��@sC�@r��@rn�@rM�@rM�@r�@q�#@qx�@qhs@qhs@q7L@q&�@q%@p��@p�9@pbN@p1'@pb@p  @o�@o��@ol�@n�R@nV@n{@m�@m��@m��@m�@l�@l�@l��@kC�@j^5@i��@i%@h�@hr�@hQ�@h �@hb@hb@hb@g��@f�@fv�@f5?@e�h@d�@c"�@b��@b�!@a��@aX@a&�@a7L@a7L@`�`@`�@` �@_�w@_K�@_�@_�@_�@_�@_
=@^�@^5?@^@]��@]O�@\��@[�@ZM�@Y�@X��@XbN@W�w@W�@WK�@V�y@Vȴ@V$�@UO�@T��@T�@S�
@SS�@R�H@R�\@R=q@Q��@Q�@Q�#@Q�^@Qhs@Q�@P�`@Pr�@P1'@P �@Pb@O�@O��@O��@OK�@N��@N�+@N@M��@M�-@M�h@M�h@M�@M/@MV@L�D@K�F@K�F@K��@K�@KdZ@KS�@J�@J=q@I�#@I��@IX@Hr�@G�w@G\)@F��@F��@FE�@F$�@E��@E�@E`B@EO�@E/@D��@D�D@D9X@D�@C��@C�m@C�F@CS�@B��@B�\@Bn�@BM�@A��@Ax�@Ahs@AX@AG�@@��@@��@@�u@@b@?�;@?\)@?
=@?
=@>��@>�y@>�@>��@>E�@>{@=@=��@=�h@=�h@=�@=`B@<��@<�@<�D@<�D@<9X@;�@;33@:�@:��@:��@:~�@:^5@:�@9��@9�@9�^@9hs@8��@8�u@8�u@8�@8�u@8�@8bN@8 �@8  @7�w@7\)@6�R@6V@65?@6$�@6@6{@6{@6{@6{@5�-@5p�@4��@41@3�@3C�@2�!@2=q@2M�@2=q@1�#@1�7@1X@1%@0�`@0��@0bN@/��@/�P@/l�@/l�@/\)@/K�@/+@/�@/
=@.�y@.ȴ@.5?@-@-�@,��@,�j@,��@,j@+�@+@*�!@*M�@*J@)��@)X@)%@(Ĝ@(r�@( �@'�w@'�P@'|�@'\)@';d@&��@&@%�h@%`B@%�@%V@%V@$�/@$�@$�@$��@$z�@$9X@#�m@#�
@#��@#S�@"^5@"J@!�@!�^@!��@!7L@ �`@ �9@ �@ 1'@   @��@\)@;d@+@+@�@�@
=@�y@�R@E�@@(�@�F@�@S�@C�@33@"�@o@@@��@n�@M�@=q@��@�#@��@�^@�7@hs@7L@��@��@r�@Q�@1'@b@  @�;@�@|�@+@��@��@V@E�@$�@@�@�h@`B@/@/@/@�@�/@�/@�/@��@��@��@�j@�D@9X@�m@t�@C�@33@�H@��@��@�!@��@�\@n�@=q@��@��@��@x�@hs@hs@X@G�@G�@&�@%@�`@��@��@bN@b@��@\)@�@��@�+@v�@ff@ff@E�@@��@@�-@�-@�-@��@��@/@�/@��@�j@�G�O�A�~�A�z�A�|�A��A�|�A�v�A�z�A��A��7A��7A��7A��7A��DA��7A��A��7A��DA��+A��A��A��7A��7A��+A��A��+A��7A��7A��A��A��+A��+A��+A��A��A��A��7A��DA��+A��A��A��A��+A��7A��+A��A��A��+A��+A��+A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�~�A�~�A��A��A��A��A��A��+A��7A��DA��7A��A��A�~�A��A��A��+A��7A��A�z�A�x�A�z�A�|�A�|�A�|�A�z�A�p�A�jA�l�A�n�A�n�A�jA�bNA�ZA�ZA�ZA�XA�VA�S�A�VA�XA�\)A�ZA�ZA�ZA�VA�S�A�S�A�VA�Q�A�Q�A�M�A�O�A�O�A�E�A�C�A�9XA�7LA�5?A�&�A�"�A�&�A�$�A��A��A�1A���A���A���A���A�A���A���A��A��7A�t�A�l�A�p�A�\)A�O�A�E�A��A�bA�JA�A�1A�A���A���A��A��TA���A���A���A���A�A��RA���A���A��uA�~�A�`BA�K�A�=qA�(�A� �A�bA��A�JA�%A��A��A��A��`A��mA��`A��`A��mA��HA�ȴA�ƨA�ĜA�A��wA��jA��^A��A���A���A���A���A��uA��A��PA��A�hsA�jA�jA�ffA�dZA�dZA�ffA�ffA�dZA�hsA�jA�hsA�hsA�jA�l�A�n�A�n�A�l�A�n�A�p�A�p�A�n�A�p�A�t�A�t�A�p�A�`BA�^5A�\)A�S�A�Q�A�O�A�M�A�I�A�K�A�I�A�?}A�1'A� �A�bA�1A�  A���A��A��`A��/A��A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA��RA��+A�K�A�A�A�;dA�5?A�5?A�33A�-A�&�A�"�A��A�{A�JA�A�A�A�A�A���A�  A�  A���A��A��`A��/A��A��
A���A���A�ȴA�ƨA�A���A���A��RA��A���A��uA���A��hA�~�A�p�A�^5A�M�A�I�A�K�A�C�A�?}A�?}A�?}A�;dA�"�A�{A�bA�bA�JA�%A�  A�  A�A���A��A��A��A��TA��;A��;A��A��A��
A���A���A���A���A���A���A���A���A�ĜA��RA��^A��RA��9A��FA��RA��FA��!A��!A��-A��9A��A���A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��PA��PA��hA��hA��7A��A��A��A�p�A�r�A�r�A�ffA�ZA�Q�A�G�A�"�A� �A�-A�(�A��A��A��A��A��A��A��A�{A�
=A���A��A��HA���A�ĜA�ĜA��wA��RA��-A���A���A���A���A���A��uA��PA��A�r�A�p�A�l�A�\)A�XA�M�A�G�A�?}A�/A�"�A��A�%A��A��HA���A���A�A���A���A���A��uA��hA��+A�|�A�n�A�dZA�`BA�`BA�^5A�XA�S�A�O�A�33A�+A��A�%A���A��`A���A���A��A�bNA�VA�G�A�9XA�$�A��A�VA���A��#A��!A���A��uA�p�A�C�A�  A��jA���A�x�A�S�A�5?A�%A�1A�ĜA��^A��jA��wA��FA���A���A�v�A�M�A�E�A�G�A�;dA�(�A�&�A� �A� �A��A��A��A�{A��A��A�bA�A��A�ƨA��!A��A���A���A��uA�jA�33A�
=A�  A���A�A�A���A�  A�  A���A���A��A��`A��
A�ĜA��jA��-A���A���A���A��uA��7A�z�A�bNA�ZA�Q�A�O�A�G�A�?}A�?}A�9XA�1'A�+A�oA�%A�A���A��A��;A���A��^A�v�A�I�A�1'A�+A��A�bA���A��A��yA��;A���A��RA���A���A��DA�XA�VA�A���A��A��/A���A�z�A�p�A�ffA�`BA�K�A�G�A�=qA�;dA�/A�(�A��A��A�{A�1A�A�  A���A���A���A���A���A���A��A���A��A��A��A��A��mA��TA��A���A�ȴA�ƨA�ĜA���A�ĜA���A��wA�A��wA��wA��RA��9A���A���A���A��PA��7A��+A�x�A�hsA�`BA�XA�M�A�/A�5?A�/A�+A��A��A��A�bA�A���A��A��mA��yA��mA��mA��yA��;A��
A���A�ĜA��wA���A���A���A���A��hA���A���A���A���A���A���A��uA��hA��A�z�A�l�A�bNA�XA�7LA��A��HA��-A���A�r�A�1'A�oA�bA��`A���A��A��hA�r�A�A�A��A���A��A���A��A�jA�`BA�XA�Q�A�O�A�K�A�9XA�VA�
=A�1A���A���A��!A���A���A�t�A�z�A�+A��A�ȴA�\)A�5?A�(�A�(�A�"�A� �A�bA�
=A��A��A�5?A�$�A�
=A���A���A��yA��yA�ȴA��wA��7A�=qA���A���A���A�\)A�O�A�Q�A�G�A��A�oA�{A�VA��TA��
A��mA�A��+A�r�A�ZA�/A� �A�{A�A��A��-A��uA���A���A�x�A�XA�^5A�+A���A��/A���A���A��FA��9A���A���A�|�A�I�A�-A��A���A���A��A�~�A�C�A��A��A�JA�A��A��HA��FA���A��uA��A�r�A�ffA�I�A��A�%A���A��/A�ƨA��^G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                              ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B!-B �B \B �B �B �B \B �B �B �B \B \B �B �B �B �B �B �B!�B#B#�B$�B'�B0!BD�BX�BjB~�B�xB�B�B��B�tB�CB��B��B��B�3B��BɺB�[B�WB�/B��B�B�`B��B�B�B�B�`B�2B��B��B��B�MB�oB��B�B�AB�B�B�
B��B��B�>B�fB��B��BیBیB�mB��B҉BбB�B�HB�)B˒BɆBȀB��B�?B��B��B��B�RB��B��B�RB�XB��B��B�B�B�eB�B��B��B{Bv�BqBh>B^�BS[BH�BC�B=�B6B0!B(�BVBB��B�B�B��B� B��B�^B�FB�eB��B�+BpBd&B[#BC�B<�B4nB \BB�B�lB��B�/B�#B�yB��B�-B��B��B�qB��B�=B�B��B�By�Bm]B_�BV�BOvBG�B?HB49B-�B'�B"�BB+B
��B
ҽB
�XB
�B
��B
��B
��B
�RB
�-B
��B
�B
��B
�:B
��B
��B
��B
.B
y�B
v`B
t�B
t�B
tB
m]B
k�B
f�B
c�B
_�B
\�B
XyB
NpB
IB
D�B
?�B
8RB
9�B
(�B
'�B
$B
�B
�B
�B
uB
"B
	B
�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	ݘB	��B	�B	ԕB	՛B	�vB	�RB	��B	�}B	�}B	��B	�B	��B	��B	��B	��B	��B	��B	�FB	��B	��B	��B	�B	��B	��B	�-B	�'B	�B	�B	�_B	��B	��B	�zB	��B	�nB	��B	�:B	�tB	�B	�4B	�-B	��B	��B	��B	�VB	�VB	��B	��B	�B	��B	�!B	��B	�B	��B	��B	�!B	��B	��B	��B	�VB	�~B	�xB	�IB	�\B	��B	�B	��B	�~B	��B	��B	�B	�~B	�!B	�OB	�hB	��B	�B	��B	��B	��B	�RB	�zB	�zB	��B	��B	��B	�_B	�0B	�B	��B	��B	��B	�?B	�FB	��B	�RB	�XB	��B	��B	�wB	��B	�'B	�[B	�[B	�[B	�-B	��B	ǮB	��B	�)B	�6B	��B	�B	�vB	ϫB	ҽB	��B	�B	خB	��B	�/B	�jB	�HB	��B	��B	�>B	�>B	�B	�QB	��B	��B	��B	�B	�%B	��B	��B	�>B
;B
�B

=B
�B
B
�B
�B
!bB
%�B
.}B
33B
49B
8�B
A B
F�B
NB
R�B
Q�B
T�B
T�B
^5B
\�B
bNB
aHB
\�B
\)B
c B
e`B
d�B
aB
_�B
^B
\�B
\)B
\]B
[�B
]dB
_pB
dZB
d�B
gmB
i�B
kB
kQB
k�B
m�B
oiB
rGB
tTB
v+B
v�B
w�B
xB
y>B
yrB
{�B
��B
�B
��B
�B
.B
��B
��B
�lB
��B
��B
��B
��B
�4B
��B
��B
��B
�+B
��B
��B
��B
�'B
��B
�B
��B
�wB
��B
�aB
�FB
�0B
�B
��B
�B
�B
�EB
��B
��B
�KB
ɆB
��B
ʌB
̘B
̘B
�NB
�NB
��B
� B
ѷB
ҽB
�&B
��B
�TB
��B
��B
�B
�WB
�)B
��B
�dB
��B
�B
�`B
�B
�B
�B
�B
��B
�)B
�cB
�B
�iB
�oB
�;B
��B
�2B
�JB
��B
�B
�"B
�VB
��B
��B
��B
��B
��B 4BB �B �BB�BBB�BfB�B	B	B
=B
	BxBB�B�B�B4B�B�B�BYBBCB�B�B 'B!-B#B$�B%zB%�B%�B'�B($B(�B*�B,�B-�B.�B/�B0�B2�B49B4nB4nB4�B5tB5�B5�B7�B8RB9�B9�B:�B:�B:^B:�B;dB;�B;dB;0B<�B=qB=�B>wB>�B?�B?}B@B@B@�B@�BA�BB�BCaBDgBC�BD3BD�BEBE�BE�BE�BF?BFBF�BFBGEBG�BGzBHBG�BG�BG�BH�BIBH�BIRBI�BI�BI�BK^BK^BK�BM�BP�BRTBR�BT,BT�BT�BT�BUgBT�BT�BT�BV9BWsBW?BV�BW�BX�BYKBYKBYKBYBX�BYBX�BYB[�B\�B]dB]�B^�B^�B^�B_;B_�B_�B`BBa�BaHBaBa�Ba�BdZBe�BgmBh
Bh>BiBh�BiyBjBi�BkQBlWBl�Bm�Bl�Bm�Bm�Bm]BncBn�Bm�Bn/Bn/Bn�Bo5Bo Bo�Bo�Bo�Bo�Bo�Bo�BpBpoBp�BqvBq�BqvBrBrBrBr|Br|Br�Bs�BtBs�Bs�Bt�Bv+Bv�Bw�Bx8Bw�Bw�Bw�By�BzDBy�BzDB{B{JB{JB{B{JB{B{B{JB{JB{�B{�B|B|�B|�B|�B|�B|�B}"B}�B}�B~]B}�B}�B~�B}�B~�B}�B~�B.B~�B�4B�4B�4B�iB� B� B�iB��B�B�oB�oB��B�oB��B��B�AB�AB�AB�B��B�{B��B��B�B�B�MB��B��B��B��B��B��B��B�%B�%B�%B��B��B��B��B��B��B��B�1B�fB��B�fB�B�1B�1B��B��B�fB��B��B�rB�rB�B�DB�~B��B��B�B��B��B��B�PB��B�VB�\B��B�\B��B��B��B�bB�.B��B��B�bB�hB�B�:B�uB�B��B��B��B��B��B�B��B�YB��B��B��B��B�_B�_B�+B�_B�+B�_B�eB��B��B��B�=B�qB�=B��B�B�B�CB��B�xB��B�xB�B��B��B�OB��B�OB��B�!B��B�\B��B��B�\B��B��B��B��B��B��B��B��B��B�hB��B��B�B�FB��B�FB�zB�zB��B��B�zB�zB��B�B��B��B�RB�B�B��B��B��B�$B�XB��B�*B��B�0B��B�eB�0B�eB�eB��B��B�B�=B�B�B�IB�IB�IB�B��B��B�B�OB�OB�OB��B��B��B�OB��B�OB��B�UB��B��B�UB�[B�'B��B��B��B��B��B��B�-B�-B��B��B��B��B��B��B��B��B�hB�hB�hB��B�B�9B�nB�tB��B��B�FB�B�B�B�FB��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B"hB 'BVB �B!�B �BOB �B!�B!-B �B 'B 'B!�B \B�B �B!bB!�B�B�B 'B!bB �B�B 'B!�B �B 'B!B \B!-B!bB �B 'B�B!bB!bB!�B �B�B�B!bB"�B!-B \B 'B�B �B!bB!-B!�B �B!-B �B!bB!bB �B!�B �B�B�B \B!�B!�B \B 'B�B �B!�B \B 'BVB �B!�B!�B!�B!bB 'B�B!B �B!-B!�B!�B �B \B�B!-B$�B#:B!�B!�B!-B!�B$@B#nB#�B#nB$@B$tB$�B$B#�B!�B#nB"�B#B#�B$@B#�B#B#�B#�B%FB#�B#nB'�B%B'�B'B'�B+�B*�B)*B)�B+�B+�B.�B4B9�B8�B7LB<�BE�BD�BK)BD�BMjBL�BI�BP}BP}BS[B]dBaHBaBc Ba�Bc Be,Bg8Bh�Bl�BsMBrBs�BsMBv+BzDB�B��B��B��B�YB�qB��B�OB�B�B��B�OB�kB��B��B��B��B�IB�IB��B�B��B��B��B�~B�B��B�~B��B�:B��B��B�B�VB�\B�FB��B�-B��B��B��B��B��B��B��B��B�B��B�B��B�OB�UB�B��B��B��B��B��B��B��B�'B��B��B��B��B��B��B�6B��B�jB��B�BB�BB�BB��B�-B��B��B�KB��B�XB�)B�6B�#B��B˒B��BɺB�RBɆB��B�#B�BȀB��B��B�#B�B�B��B�BںB��B�B��B��B�)B�)BیB��B�dB�5B��B��B�]B�/B�B��B��B�5B�BޞB�BB�B��B�B�B�B�HB�B��B�B�B�mB�B�B�&B��B�fB�B��B�
B��B��B�iB��B�B�B�B��B��B��B�"B�B�)B�5B�]B��B��B�B�;B��B�B�B��B�+B��B��B��B��B��B��B��B��B��B�ZB��B�B��B��B�fB��B��B��B��B��B�ZB��B�lB��B��B��B��B�+B��B�%B��B��B��B��B�B�ZB�B��B��B�B�MB�|B��B�B��B�B�iB�vB��B�;B�B�B�B�/B��B�yB�B��B�B�B�B�GB��B�B�B�B�B��B��B�B�iB�)B��B�B��B�"B��B�/B�B�DB�"B�KB�KB��B�B�B�B��B�B�B�
B�fB�8B�B�B�B��B��B�B�B�,B�B�KB�2B��B�B�mB�
B�KB�sB�mB�sB��B��B�fB�2B��B�B��B��B�B�B�mB��B�B�B��BیB�/B�dBݘBچB��B��B��BܒB��B�yB��BںB��BܒB�[B�HB�B��BܒB��B��B�[B�,B�[B��BںBҽB��B՛B�B�}B��B҉BѷB�}BѷB�TB��B�BҽB�B�B��B�vB�gBԕB�BΥBϫB�B�}BԕB�B��B��B�dB��B��B�0B��B��B�XB��B��B�dB��B̘B�#BʌB�RB�XB�RBȀB�B�XB˒B�B�EB�EB�B�EB�B�tBƨB�EB��B�tB��B�?B�9B��B��B�zB�B�B�'B��B�UB�'B�[B��B��B��B�-B�UB��B�wB�wB�XB�OB��B�qB�0B��B��B��B��B��B��B�dB��B�^B�$B�0B��B�^B�*B��B�^B�B�XB��B�B�B��B��B�B��B�B��B��B��B��B��B��B�$B��B��B��B�LB�RB�FB�LB�RB�zB��B��B�RB�RB��B�$B�zB�zB��B��B�zB��B��B�RB��B�B��B�B�B��B��B�$B��B�FB��B�zB��B�$B��B��B�*B��B�jB��B��B��B��B�B��B��B�0B��B��B��B��B�<B�<B�BB�B�}B�OB��B��B�BB�BB��B��B��B�aB�B��B�B�_B�qB��B��B��B�hB��B��B��B��B��B�_B��B�B��B�B��B��B��B��B��B�kB��B�B��B��B�OB��B��B��B�B��B�hB�B��B�bB�bB�hB�B��B��B�'B�PB�B�	B��B�SB�+B�B��B�{B��B��B��B|PB�MBzDBzDBzxB{B��By	Bv�Bx8B�%Bt�BuZBzB��By	Bv+B}"BsBt�BqABrGBu�BpoBp�BpoBn�Bx8Bo5Bs�BqBl�Bj�Bi�BiBf2Bg8BhsBgBiDBc�Bc Be`Bf�B_pBc�Bc BXEBT�BV�BV�BVBW
BW
BU2BP}BR�BOvBMjBQ�BNBMBK)BL�BHKBIRG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     none                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     none                                                                                                                                                                                                                                                            202209061814042022090618140420220906181404202209061814042022090618140420220906181404SI  SI  ARFMARFM                                                                                                                                                2021041411212420210414112124IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021042700040220210427000402QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021042700040220210427000402QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021101413074620211014130746IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618140720220906181407IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618140720220906181407IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618140720220906181407IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                