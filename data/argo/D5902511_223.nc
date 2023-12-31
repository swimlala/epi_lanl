CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  :   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-08-06T01:16:05Z creation; 2023-02-10T23:09:44Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  V�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  ]L   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  w   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  }�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  �0   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t  �t   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 t +�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 2@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` L   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   Lp   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   Rp   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   Xp   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ^p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ^�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ^�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ^�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ^�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ^�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   _d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   _�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    _�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        _�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        _�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       _�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    _�Argo profile    3.1 1.2 19500101000000  20220806011605  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_223                 6810_008521_223                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�伹M�@�伹M�11  @����[@����[@1�oa�@1�oa��d�����
�d�����
11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@��\@�  @��R@�  @��RA�RA ��A,(�A@  AaG�A�  A�\)A�\)A�Q�A�Q�A�  A߮A�  B (�B(�B  B�
B   B(  B/�
B7�
B?�BG�BP  BX  B`(�Bh  Bo�
Bw�
B�{B�{B��B�  B�  B�  B��B�  B�{B�  B�  B�{B�  B�{B�  B�  B�  B��B�  B�(�B�{B�{B�{B��B��
B��B�  B�{B�  B��B�  B��
C   C  C  C
=C  C	��C  C��C
=C  C�C�C�C�C  C  C��C"  C#��C&  C(  C)��C,  C.  C0
=C2{C4
=C6
=C8
=C:
=C<  C=�C@  CA��CC��CE��CG�CJ
=CL  CM��CP  CR
=CT  CV  CX  CY��C\
=C]��C`  Cb  Cc��Cf  Ch  Cj  Cl  Cn  Co��Cq�Cs�Cu��Cx  Cz  C|  C~
=C�
=C�
=C�C�
=C�  C���C�  C�  C�  C�
=C�
=C�C�C�C�  C���C���C���C�  C�
=C�
=C�
=C�C�  C�  C�C�C�  C���C���C�
=C�C�  C���C�C�
=C�  C�  C�  C�  C�C�C�  C�C�  C���C���C���C�  C���C�C�  C���C���C���C�C�  C�  C�C�  C�  C�C�  C�  C�C���C�  C�
=C�  C���C�C�  C���C�C�  C���C�  C�  C�  C���C��C���C���C���C���C�C�\C�
=C�C���C���C�  C�  C�  C�C�  C���C���C���C���C���C�  C�  C���C�
=C���C���C���C�  C�  C���C���C�C�C�  C���C���C���C���C�  C�  C���C�  C�  C�C�C�C�D   D ��D  D}qD�D�DD��D�D� D  D}qD�qD� D�D��D  D}qD	  D	� D
  D
}qD  D��D�D��D�D}qD�qD}qD�qD��D�D��DD�D�D��DD��D�D��D�D� D  D� DD��D�qD}qD�D��D�D��D  D� D  D}qD  D}qD  D� D�qD� D   D � D!�D!��D"�D"��D#�D#� D$  D$��D%�D%� D&  D&}qD'  D'� D(  D(}qD(�qD)}qD*  D*� D*�qD+� D,  D,}qD,�qD-z�D.  D.}qD.��D/� D0�D0� D0�qD1}qD2  D2�D3  D3}qD4  D4� D5  D5}qD5�qD6}qD7�D7� D8  D8��D9  D9��D:D:��D;  D;� D<  D<�D=D=� D>  D>��D?  D?� D?�qD@z�D@�qDA� DB  DB� DC  DC��DDDD}qDE  DE� DE�qDF� DG�DG� DG�qDHz�DI  DI�DJ�DJ��DKDK��DL�DL� DM�DM� DM�qDN� DO�DO��DP�DP� DP�qDQ� DR  DR� DS�DS�DT�DT� DU  DU��DV  DV� DW  DW� DX  DX� DY  DY}qDY�qDZ� D[�D[� D\  D\� D\�qD]z�D^  D^� D^�qD_}qD`  D`}qDa  Da��Da�qDbz�Db�qDc� Dd  Dd� De�De� Df  Df��DgDg� Dh  Dh� Di  Di}qDj  Dj��Dk�Dk}qDk�qDl}qDm  Dm��Dn�Dn��Do�Do��Dp�Dp� Dq  Dq� Dr  Drz�Dr�qDs}qDs�qDt}qDu  Du��Du�qDv}qDw  Dw� Dw�qDx� Dy  Dy��Dz  Dz� D{  D{}qD|  D|�D}�D}}qD}�qD~z�D~��D}qD�  D�AHD���D�� D�  D�@ D�~�D���D�  D�@ D�~�D���D�  D�@ D�� D�� D�HD�@ D���D�D�  D�>�D��HD�D�HD�B�D��HD�� D�  D�>�D�}qD�� D�
=D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D��=D�� G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��
?��?u?���?Ǯ?��H@�@#�
@@  @Q�@c�
@�G�@��@�33@�  @���@�z�@\@���@�@��
@�\)@�
=A�\A��Ap�A�\A��A   A$z�A*=qA1G�A5A:�HAA�AG�AL(�AR�\AX��A^{Ab�\Ai��Ao\)As�
Az�HA�Q�A��\A�A���A��HA�{A�G�A��A�ffA���A��
A��RA��A��
A�
=A�=qA�z�A�\)A��\A���A��A��HA�p�A�  AÅA�Aȣ�A�(�AθRA���A�(�A�
=A�G�A���A�\)AᙚA�z�A�\)A陚A���A�A��A���A��A�=qA�z�A��BG�BffB�
Bp�B�\B  B	B
�RB(�BB33B(�Bp�B
=B(�BG�B�RB(�BG�B{B�B��BB�\B   B ��B!��B"�RB#�
B$z�B%G�B&�\B'
=B'�B(��B)B*ffB*�HB+�
B,��B-�B.{B/
=B/�B0Q�B1G�B2{B2�RB3\)B4z�B5�B5��B6�RB7\)B8  B8��B9�B:ffB;\)B<Q�B<��B=B>�RB?\)B@  BA�BA�BBffBC�BDQ�BD��BE��BF�RBG\)BG�
BH��BI�BJ�\BK33BLQ�BL��BM��BN�RBO�BP  BP��BQ�BR�\BS
=BT(�BU�BUp�BVffBW�BW�
BX��BYBZ�\B[
=B\  B\��B]p�B^=qB_\)B`  B`��Bap�Bb�\Bc\)Bc�
Bd��Be�BfffBg
=Bh(�Bh��Bip�Bj�\Bk�Bl(�Bl��Bm�Bn�RBo33Bp  BqG�Bq�Br�\Bs�Btz�Bt��Bv{Bv�HBw\)Bxz�Byp�Bz{Bz�RB|  B|��B}G�B~=qB\)B�
B�Q�B���B�G�B��B��B��\B���B��B�B�(�B�z�B��RB�\)B�B�{B�ffB�
=B�\)B���B�(�B���B���B�G�B��
B�=qB�z�B���B�p�B��B�=qB���B�G�B��B�  B�ffB���B�\)B��B�{B��RB���B�\)B��B�ffB��RB�
=B��B�{B�Q�B��RB�G�B�B�{B�z�B��B�\)B��B�ffB��RB�
=B���B�{B�Q�B��RB�G�B��B�  B�ffB���B�p�B��B�(�B��\B��B�\)B�B�Q�B���B�33B�p�B�  B�z�B��HB�G�B���B�(�B���B��B�\)B�B�ffB���B�
=B���B�(�B�z�B��RB�G�B��
B�{B�ffB���B�p�B�B�{B�z�B�
=B�p�B��
B�{B�z�B�
=B�p�B�B�(�B���B�
=B�\)B�  B�ffB���B��B���B�{B�ffB���B�33B��B��B�Q�B��HB�\)B��B�  B���B��B�\)B��
B�ffB���B��B���B�(�B���B��HB�\)B�  B�Q�B���B�
=B���B�{B�ffB���B�\)B��B�  B�z�B���B�p�B��
B�(�Bģ�B��BŮB�  B�Q�B���B�\)B�B�{B�z�B�
=B�p�B�B�=qB���B�G�B˙�B��B�ffB��HB�p�B��
B�{BΣ�B��Bϙ�B��
B�=qB��HB�G�Bљ�B�{Bң�B��B�p�B��
B�ffB��HB�\)BծB�(�B���B�33BׅB�  B؏\B��B�p�B�B�Q�B��HB�G�BۮB�  B܏\B�
=Bݙ�B�  B�Q�B޸RB�G�B��
B�=qB��\B���B�B�{B�\B���B�\)B�  B�z�B���B�G�B��B�z�B���B�33B��
B�ffB��HB�33B陚B�  B�\B�33B�B��B�ffB���B�p�B��
B�=qB��B�33B�B�(�B�z�B��HB�B�  B�z�B���B�33B�B�Q�B���B���B��B�=qB��RB��B��B�  B��\B��B��B�{B�z�B���B���B�{B�z�B��HB�p�B�  B�ffB���B�G�B��
C 33C z�C �C �C{CQ�C��C�C{CG�C�C�
C{CG�Cz�C�RC
=CQ�Cp�C��C��C=qCz�C�C�HC�Cp�C�C�HC{C\)C�C�C{CQ�C��C�C	(�C	Q�C	��C	�HC
�C
G�C
�\C
��C�CQ�C�CC{C\)C�C��C{CQ�C�C�RC��CG�C�\C�
C  C=qC�\C��C��C=qC�\C��C��C33Cp�CC  C33C\)C�\C�HC(�C\)Cz�C�RC
=C=qCffC��C�HC�C=qCz�C�RC  C�CG�C�\C�
C{C33CffC��C�C(�CQ�C�\C�HC(�CG�C�C��C�CQ�Cz�C��C�CQ�C�C�RC
=CG�C�C�C�HC(�Cp�C�C�HC
=CG�C�\C��C
=C33CffC�C��C (�C \)C �\C �
C!�C!ffC!��C!��C"  C"=qC"�\C"�
C#{C#=qC#p�C#�RC$
=C$=qC$p�C$��C$�C%33C%ffC%�\C%�HC&(�C&\)C&�\C&��C'�C'ffC'��C'��C(  C(=qC(�C(��C)
=C)=qC)z�C)�C)�C*=qC*�C*�RC*�C+�C+\)C+��C+�C,(�C,ffC,��C,��C-
=C-Q�C-�\C-�
C.
=C.33C.p�C.�C.�C/33C/p�C/��C/C0{C0ffC0��C0��C1  C133C1p�C1C2  C2G�C2�\C2C2��C333C3z�C3C4
=C4G�C4�C4�RC4�C5(�C5p�C5�RC6  C6G�C6p�C6��C6�C7(�C7p�C7C8  C8=qC8z�C8�RC8�C9�C9ffC9��C9��C:33C:p�C:��C:�
C;�C;\)C;��C;��C<33C<z�C<C=  C=G�C=p�C=�C=�C>33C>p�C>�RC?
=C?Q�C?�\C?�
C@{C@Q�C@�C@CA  CAG�CA�CA��CB�CBffCB�CB�CC33CCffCC��CC�HCD{CDffCD�CD��CE(�CEffCE��CE�CF33CFz�CF��CG{CGQ�CG�CGCH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                        1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?�  @   @B�\@��\@�  @��R@�  @��RA�RA ��A,(�A@  AaG�A�  A�\)A�\)A�Q�A�Q�A�  A߮A�  B (�B(�B  B�
B   B(  B/�
B7�
B?�BG�BP  BX  B`(�Bh  Bo�
Bw�
B�{B�{B��B�  B�  B�  B��B�  B�{B�  B�  B�{B�  B�{B�  B�  B�  B��B�  B�(�B�{B�{B�{B��B��
B��B�  B�{B�  B��B�  B��
C   C  C  C
=C  C	��C  C��C
=C  C�C�C�C�C  C  C��C"  C#��C&  C(  C)��C,  C.  C0
=C2{C4
=C6
=C8
=C:
=C<  C=�C@  CA��CC��CE��CG�CJ
=CL  CM��CP  CR
=CT  CV  CX  CY��C\
=C]��C`  Cb  Cc��Cf  Ch  Cj  Cl  Cn  Co��Cq�Cs�Cu��Cx  Cz  C|  C~
=C�
=C�
=C�C�
=C�  C���C�  C�  C�  C�
=C�
=C�C�C�C�  C���C���C���C�  C�
=C�
=C�
=C�C�  C�  C�C�C�  C���C���C�
=C�C�  C���C�C�
=C�  C�  C�  C�  C�C�C�  C�C�  C���C���C���C�  C���C�C�  C���C���C���C�C�  C�  C�C�  C�  C�C�  C�  C�C���C�  C�
=C�  C���C�C�  C���C�C�  C���C�  C�  C�  C���C��C���C���C���C���C�C�\C�
=C�C���C���C�  C�  C�  C�C�  C���C���C���C���C���C�  C�  C���C�
=C���C���C���C�  C�  C���C���C�C�C�  C���C���C���C���C�  C�  C���C�  C�  C�C�C�C�D   D ��D  D}qD�D�DD��D�D� D  D}qD�qD� D�D��D  D}qD	  D	� D
  D
}qD  D��D�D��D�D}qD�qD}qD�qD��D�D��DD�D�D��DD��D�D��D�D� D  D� DD��D�qD}qD�D��D�D��D  D� D  D}qD  D}qD  D� D�qD� D   D � D!�D!��D"�D"��D#�D#� D$  D$��D%�D%� D&  D&}qD'  D'� D(  D(}qD(�qD)}qD*  D*� D*�qD+� D,  D,}qD,�qD-z�D.  D.}qD.��D/� D0�D0� D0�qD1}qD2  D2�D3  D3}qD4  D4� D5  D5}qD5�qD6}qD7�D7� D8  D8��D9  D9��D:D:��D;  D;� D<  D<�D=D=� D>  D>��D?  D?� D?�qD@z�D@�qDA� DB  DB� DC  DC��DDDD}qDE  DE� DE�qDF� DG�DG� DG�qDHz�DI  DI�DJ�DJ��DKDK��DL�DL� DM�DM� DM�qDN� DO�DO��DP�DP� DP�qDQ� DR  DR� DS�DS�DT�DT� DU  DU��DV  DV� DW  DW� DX  DX� DY  DY}qDY�qDZ� D[�D[� D\  D\� D\�qD]z�D^  D^� D^�qD_}qD`  D`}qDa  Da��Da�qDbz�Db�qDc� Dd  Dd� De�De� Df  Df��DgDg� Dh  Dh� Di  Di}qDj  Dj��Dk�Dk}qDk�qDl}qDm  Dm��Dn�Dn��Do�Do��Dp�Dp� Dq  Dq� Dr  Drz�Dr�qDs}qDs�qDt}qDu  Du��Du�qDv}qDw  Dw� Dw�qDx� Dy  Dy��Dz  Dz� D{  D{}qD|  D|�D}�D}}qD}�qD~z�D~��D}qD�  D�AHD���D�� D�  D�@ D�~�D���D�  D�@ D�~�D���D�  D�@ D�� D�� D�HD�@ D���D�D�  D�>�D��HD�D�HD�B�D��HD�� D�  D�>�D�}qD�� D�
=D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D��=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>��
?��?u?���?Ǯ?��H@�@#�
@@  @Q�@c�
@�G�@��@�33@�  @���@�z�@\@���@�@��
@�\)@�
=A�\A��Ap�A�\A��A   A$z�A*=qA1G�A5A:�HAA�AG�AL(�AR�\AX��A^{Ab�\Ai��Ao\)As�
Az�HA�Q�A��\A�A���A��HA�{A�G�A��A�ffA���A��
A��RA��A��
A�
=A�=qA�z�A�\)A��\A���A��A��HA�p�A�  AÅA�Aȣ�A�(�AθRA���A�(�A�
=A�G�A���A�\)AᙚA�z�A�\)A陚A���A�A��A���A��A�=qA�z�A��BG�BffB�
Bp�B�\B  B	B
�RB(�BB33B(�Bp�B
=B(�BG�B�RB(�BG�B{B�B��BB�\B   B ��B!��B"�RB#�
B$z�B%G�B&�\B'
=B'�B(��B)B*ffB*�HB+�
B,��B-�B.{B/
=B/�B0Q�B1G�B2{B2�RB3\)B4z�B5�B5��B6�RB7\)B8  B8��B9�B:ffB;\)B<Q�B<��B=B>�RB?\)B@  BA�BA�BBffBC�BDQ�BD��BE��BF�RBG\)BG�
BH��BI�BJ�\BK33BLQ�BL��BM��BN�RBO�BP  BP��BQ�BR�\BS
=BT(�BU�BUp�BVffBW�BW�
BX��BYBZ�\B[
=B\  B\��B]p�B^=qB_\)B`  B`��Bap�Bb�\Bc\)Bc�
Bd��Be�BfffBg
=Bh(�Bh��Bip�Bj�\Bk�Bl(�Bl��Bm�Bn�RBo33Bp  BqG�Bq�Br�\Bs�Btz�Bt��Bv{Bv�HBw\)Bxz�Byp�Bz{Bz�RB|  B|��B}G�B~=qB\)B�
B�Q�B���B�G�B��B��B��\B���B��B�B�(�B�z�B��RB�\)B�B�{B�ffB�
=B�\)B���B�(�B���B���B�G�B��
B�=qB�z�B���B�p�B��B�=qB���B�G�B��B�  B�ffB���B�\)B��B�{B��RB���B�\)B��B�ffB��RB�
=B��B�{B�Q�B��RB�G�B�B�{B�z�B��B�\)B��B�ffB��RB�
=B���B�{B�Q�B��RB�G�B��B�  B�ffB���B�p�B��B�(�B��\B��B�\)B�B�Q�B���B�33B�p�B�  B�z�B��HB�G�B���B�(�B���B��B�\)B�B�ffB���B�
=B���B�(�B�z�B��RB�G�B��
B�{B�ffB���B�p�B�B�{B�z�B�
=B�p�B��
B�{B�z�B�
=B�p�B�B�(�B���B�
=B�\)B�  B�ffB���B��B���B�{B�ffB���B�33B��B��B�Q�B��HB�\)B��B�  B���B��B�\)B��
B�ffB���B��B���B�(�B���B��HB�\)B�  B�Q�B���B�
=B���B�{B�ffB���B�\)B��B�  B�z�B���B�p�B��
B�(�Bģ�B��BŮB�  B�Q�B���B�\)B�B�{B�z�B�
=B�p�B�B�=qB���B�G�B˙�B��B�ffB��HB�p�B��
B�{BΣ�B��Bϙ�B��
B�=qB��HB�G�Bљ�B�{Bң�B��B�p�B��
B�ffB��HB�\)BծB�(�B���B�33BׅB�  B؏\B��B�p�B�B�Q�B��HB�G�BۮB�  B܏\B�
=Bݙ�B�  B�Q�B޸RB�G�B��
B�=qB��\B���B�B�{B�\B���B�\)B�  B�z�B���B�G�B��B�z�B���B�33B��
B�ffB��HB�33B陚B�  B�\B�33B�B��B�ffB���B�p�B��
B�=qB��B�33B�B�(�B�z�B��HB�B�  B�z�B���B�33B�B�Q�B���B���B��B�=qB��RB��B��B�  B��\B��B��B�{B�z�B���B���B�{B�z�B��HB�p�B�  B�ffB���B�G�B��
C 33C z�C �C �C{CQ�C��C�C{CG�C�C�
C{CG�Cz�C�RC
=CQ�Cp�C��C��C=qCz�C�C�HC�Cp�C�C�HC{C\)C�C�C{CQ�C��C�C	(�C	Q�C	��C	�HC
�C
G�C
�\C
��C�CQ�C�CC{C\)C�C��C{CQ�C�C�RC��CG�C�\C�
C  C=qC�\C��C��C=qC�\C��C��C33Cp�CC  C33C\)C�\C�HC(�C\)Cz�C�RC
=C=qCffC��C�HC�C=qCz�C�RC  C�CG�C�\C�
C{C33CffC��C�C(�CQ�C�\C�HC(�CG�C�C��C�CQ�Cz�C��C�CQ�C�C�RC
=CG�C�C�C�HC(�Cp�C�C�HC
=CG�C�\C��C
=C33CffC�C��C (�C \)C �\C �
C!�C!ffC!��C!��C"  C"=qC"�\C"�
C#{C#=qC#p�C#�RC$
=C$=qC$p�C$��C$�C%33C%ffC%�\C%�HC&(�C&\)C&�\C&��C'�C'ffC'��C'��C(  C(=qC(�C(��C)
=C)=qC)z�C)�C)�C*=qC*�C*�RC*�C+�C+\)C+��C+�C,(�C,ffC,��C,��C-
=C-Q�C-�\C-�
C.
=C.33C.p�C.�C.�C/33C/p�C/��C/C0{C0ffC0��C0��C1  C133C1p�C1C2  C2G�C2�\C2C2��C333C3z�C3C4
=C4G�C4�C4�RC4�C5(�C5p�C5�RC6  C6G�C6p�C6��C6�C7(�C7p�C7C8  C8=qC8z�C8�RC8�C9�C9ffC9��C9��C:33C:p�C:��C:�
C;�C;\)C;��C;��C<33C<z�C<C=  C=G�C=p�C=�C=�C>33C>p�C>�RC?
=C?Q�C?�\C?�
C@{C@Q�C@�C@CA  CAG�CA�CA��CB�CBffCB�CB�CC33CCffCC��CC�HCD{CDffCD�CD��CE(�CEffCE��CE�CF33CFz�CF��CG{CGQ�CG�CGCH
=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                        1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�A���AڬAڕ�Aڇ+A�XA�VA���AٶFA٩�A١�Aٟ�Aٙ�AٓuAٍPAمAفA�|�A�z�A�x�A�v�A�r�A�n�A�jA�ffA�`BA�VA�K�A�E�A�A�A�A�A�=qA�/A� �A�oA��AؾwA�$�Aק�A�G�Aֺ^A��mAԡ�A� �Aӗ�A�G�A��A�t�A�9XA���A�`BA���A���A�\)A��A�bA���A�^5A��A�M�Aɛ�A�"�A�1A���A�bNA�A��/Aŉ7A�{A�p�A��#A��^A�A�9XA��A�M�A�ĜA��A�oA��uA��hA�7LA�  A��PA�%A�I�A��-A��/A�5?A��A�S�A��`A�%A��A�{A��RA���A�x�A���A�|�A�;dA���A��A��\A�"�A���A��hA�$�A��A�5?A��;A�~�A�t�A�S�A��hA�33A}`BAz�Ax�uAu�#AsAp�jAl�+AkoAj�`Ah��AgXAfI�AdbNAb��A`�A^ZA\�yA[��AY�AW�TAV�HAT=qAS�^AR~�AN�AL�RAK|�AJ�jAJ�AHĜAFE�AD�AB�/AA�;A@��A>z�A=dZA<jA:�A8��A7A6bNA5��A5VA3��A2v�A0��A-��A-oA,bA)��A(��A'A%O�A#33A!�A��A$�AdZA?}A�!Ax�AZA�mAl�AoA^5A�hAjA�FA;dAVA�A;dA=qA&�A
1'A	�^A	;dA�yA��A��A��A�-A�jAC�A�wA ��@��F@�=q@�G�@�j@��P@�o@�O�@��F@��!@��@�;d@�@�n�@�u@�1@�V@�D@�R@�$�@��@��@�7@���@� �@ߝ�@��@�7L@��@�b@�~�@�O�@�I�@���@� �@��@ӕ�@�j@�+@�J@�n�@��@љ�@���@�j@�S�@�-@��@ͩ�@�`B@̴9@���@�l�@�C�@ʇ+@���@�n�@�J@�p�@��@�?}@�@̋D@��@�5?@�ff@Η�@���@ΰ!@Η�@�@̓u@�;d@�+@�
=@���@�^5@�`B@�9X@ǅ@�C�@�M�@�X@ř�@�@ċD@î@�|�@�+@�"�@��@�+@��@�n�@�{@��h@���@���@�Ĝ@��@�Z@��;@��F@�C�@���@���@�~�@�5?@��@���@�&�@��9@�z�@�Z@�9X@��;@��@�v�@��@���@�G�@��@��@�9X@�b@��@�"�@���@��@���@�X@�?}@��@���@�b@���@�;d@��@��H@��+@�ff@�{@��@���@�V@�j@�9X@�(�@�b@��
@���@�K�@��!@�^5@��@��-@��@�X@�V@�Z@�1@��m@�ƨ@�|�@�+@���@�E�@�@��-@�?}@��@� �@��@�C�@���@���@���@�~�@�^5@�{@��h@�X@�/@��`@���@�Q�@��@�b@�1@���@��;@�ƨ@��@��@��y@���@�-@�`B@��`@�Z@�1'@�  @��;@���@�\)@�S�@�K�@�+@�ȴ@�V@�J@��T@���@�X@�V@���@�r�@�9X@��m@��@�t�@�dZ@�\)@�\)@�K�@�33@�{@��h@�p�@�hs@�O�@�/@��@���@���@���@���@��/@��9@���@�Z@�1@���@�K�@��H@���@�~�@�5?@�@��#@��^@��7@�G�@���@��@�j@�(�@���@�S�@�o@�ȴ@�V@��^@�X@��@��u@�Q�@���@��F@��F@���@�|�@�\)@��@�ȴ@���@�=q@��#@��@�X@�&�@��@���@�j@�A�@� �@��@���@�l�@�@���@�ff@�E�@��@���@���@�p�@�%@��j@��D@�I�@���@��F@��P@�K�@�"�@���@��@��!@�n�@�E�@�-@���@��-@�x�@��@��@��/@��/@��/@���@�z�@�9X@��@��w@�S�@�
=@��!@�-@�J@��@��@�7L@�%@��9@��@��u@�r�@� �@�@�w@\)@~��@~{@}�T@}�@|�@|�j@|z�@|9X@{33@z^5@y��@y�7@xĜ@x  @wK�@v�+@vE�@vE�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�oA��A��`Aڰ!Aڧ�AڶFAڡ�AړuAڕ�AڑhAڍPA�x�A�x�A�jA�+A�$�A�1A��yA��HA���A�ƨAٴ9AٶFAٲ-A٩�A٧�A٧�A٣�Aٝ�Aٝ�Aٟ�Aٛ�Aٙ�Aٝ�Aٛ�AٓuAٕ�Aٗ�AٓuAُ\AّhAّhAٍPAُ\Aُ\Aى7AًDAًDAكAمAمAفAكAمA�~�A�~�AكA�|�A�~�AفA�|�A�|�AفA�z�A�z�A�~�A�z�A�v�A�|�A�x�A�v�A�|�A�x�A�t�A�z�A�x�A�t�A�x�A�x�A�r�A�v�A�t�A�p�A�r�A�t�A�n�A�p�A�t�A�p�A�l�A�p�A�n�A�l�A�l�A�n�A�hsA�hsA�l�A�ffA�ffA�jA�jA�dZA�ffA�dZA�`BA�^5A�bNA�dZA�bNA�^5A�`BA�`BA�XA�S�A�VA�Q�A�O�A�Q�A�VA�M�A�K�A�O�A�K�A�I�A�I�A�M�A�K�A�E�A�I�A�G�A�C�A�G�A�I�A�E�A�C�A�E�A�G�A�C�A�C�A�G�A�A�A�?}A�C�A�A�A�=qA�A�A�C�A�?}A�A�A�E�A�?}A�A�A�E�A�?}A�=qA�A�A�A�A�=qA�A�A�A�A�=qA�=qA�C�A�?}A�;dA�=qA�?}A�9XA�5?A�5?A�/A�-A�1'A�1'A�+A�-A�/A�-A�(�A�-A�(�A�"�A�"�A�$�A� �A��A� �A��A��A��A��A�{A�{A��A�oA�VA�bA�{A�oA�%A�  A���A��A��yA��A��A��`A��`A��`A��HA��#A��#A��
A�ĜA���A���Aذ!Aؗ�A�p�A�\)A�G�A�=qA�7LA�1'A�-A�oA�A��A���A���A׶FA׸RA׶FAץ�Aן�Aף�Aף�AדuAׅA�|�A�l�A�bNA�bNA�XA�K�A�"�A�oA�A��A��#A�ĜA־wAֶFAֶFAֶFA֧�A֛�A֓uA։7A�n�A�dZA�G�A�oA��HAՑhA�Q�A�&�A�A��;A���AԴ9Aԙ�AԓuAԕ�AԃA�x�A�VA�O�A�C�A�1'A�"�A� �A�JA�
=A�
=A�  A��A��A��Aӧ�A�|�A�t�A�n�A�dZA�^5A�^5A�XA�VA�S�A�M�A�A�A�=qA�;dA�9XA�1'A�/A�+A�-A�&�A�$�A�$�A�$�A��A���A��HA�Aқ�A҅A�n�A�jA�dZA�S�A�?}A�=qA�?}A�=qA�7LA�9XA�;dA�;dA�5?A�9XA�9XA�-A��A��A��A�oA�%A���A��/A���Aѣ�Aї�Aѕ�Aѕ�AыDA�n�A�S�A�;dA�/A�&�A�(�A��A�JA�%A��`A�ĜAд9AУ�AУ�AН�A�v�A�E�A�33A�&�A��A�bA�A���AϑhAϋDAχ+A�z�A�l�A�`BA�XA�Q�A�C�A�;dA�5?A�(�A��A�1A���A���A��`A��#A���Aδ9AΙ�AΉ7A�x�A�K�A� �A���A���Aͧ�A�x�A�O�A�5?A��A�oA�A���A��`A��`A���A̬A̓uA�|�A�t�A�l�A�dZA�`BA�Q�A�=qA�-A��A�z�A�C�A�&�A�A���A��HA���A���A�ĜA�ƨA���AʑhA�;dA�(�A���A��mA��A���A���Aɴ9Aɛ�AɍPAɍPAɉ7A�~�A�p�A�hsA�ZA�+A��A�JA�A���A���A��yA�ƨAȑhA�C�A���A�ĜAǩ�A�x�A�G�A�5?A��A�bA�%A�A��A��A��/A�Aƥ�AƉ7A�p�A�bNA�ZA�K�A�9XA�&�A��A��A�bA�A���A���A���A���A��A��mA��HA��;A��HA��;A��/A��
A���A�AžwAź^Aź^AżjAŮA�Q�A�-A� �A�1A��;Ağ�A�-Aò-AÛ�AÉ7A�x�A�x�A�n�A�r�A�v�A�t�A�p�A�hsA�l�A�n�A�\)A�/A� �A���A���AhA�\)A�/A��A�A��A���A��9A��\A�|�A�hsA�O�A�9XA�-A�"�A��A�A��TA��RA��7A�K�A���A�bNA�33A��TA��FA��A���A���A���A���A��7A�t�A�l�A�bNA�\)A�ZA�S�A�Q�A�S�A�O�A�G�A�=qA�?}A�33A�"�A�oA��A��FA�x�A�n�A�hsA�jA�XA�;dA��A��yA�A��+A�ffA�K�A�1'A�&�A��A�bA�
=A�A���A��#A���A��!A���A���A��DA��A�r�A�O�A�-A�oA��A��yA��
A���A�/A��PA�7LA���A��uA�n�A�K�A�"�A���A��A��TA�ƨA���A�$�A��yA�dZA���A��\A�1'A��A�S�A��-A��DA�VA�
=A��#A� �A�oA��#A��9A��7A�C�A�bA��A���A��FA���A���A��A�p�A�hsA�S�A�;dA�(�A�"�A��A���A�ȴA��A�I�A���A���A�n�A�|�A�{A�r�A�A�A�"�A�A��/A���A��A�dZA�XA�G�A�E�A�A�A�A�A�?}A�/A�JA�A��yA��/A��
A��wA���A��DA�r�A�bNA�C�A��yA��A�E�A�1'A��A��A���A���A��A�`BA�?}A�JA��/A���A��A��jA���A�r�A�33A�%A��
A�A���A���A���A�~�A�?}A�{A��A���A���A�x�A�O�A��A���A�jA��A��mA��RA��uA��7A�t�A�I�A���A�&�A��9A�1'A�ȴA���A�z�A�dZA�Q�A�(�A��yA��PA�JA��HA�`BA��-A���A��uA��7A�n�A�M�A�;dA�$�A�&�A�&�A��A�JA�  A��;A�ĜA��9A���A���A���A���A���A��hA�?}A�
=A��jA�r�A�A�A�7LA�"�A�VA�A�  A��A��;A�A���A��7A�hsA�Q�A�=qA�-A��jA�+A��DA�p�A�oA��#A���A��A���A��uA���A��A�jA�\)A�VA�M�A�5?A��A�VA�%A�  A��A���A�ƨA��RA���A��hA��A�x�A�hsA�I�A��`A���A�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                        1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���AڬAڕ�Aڇ+A�XA�VA���AٶFA٩�A١�Aٟ�Aٙ�AٓuAٍPAمAفA�|�A�z�A�x�A�v�A�r�A�n�A�jA�ffA�`BA�VA�K�A�E�A�A�A�A�A�=qA�/A� �A�oA��AؾwA�$�Aק�A�G�Aֺ^A��mAԡ�A� �Aӗ�A�G�A��A�t�A�9XA���A�`BA���A���A�\)A��A�bA���A�^5A��A�M�Aɛ�A�"�A�1A���A�bNA�A��/Aŉ7A�{A�p�A��#A��^A�A�9XA��A�M�A�ĜA��A�oA��uA��hA�7LA�  A��PA�%A�I�A��-A��/A�5?A��A�S�A��`A�%A��A�{A��RA���A�x�A���A�|�A�;dA���A��A��\A�"�A���A��hA�$�A��A�5?A��;A�~�A�t�A�S�A��hA�33A}`BAz�Ax�uAu�#AsAp�jAl�+AkoAj�`Ah��AgXAfI�AdbNAb��A`�A^ZA\�yA[��AY�AW�TAV�HAT=qAS�^AR~�AN�AL�RAK|�AJ�jAJ�AHĜAFE�AD�AB�/AA�;A@��A>z�A=dZA<jA:�A8��A7A6bNA5��A5VA3��A2v�A0��A-��A-oA,bA)��A(��A'A%O�A#33A!�A��A$�AdZA?}A�!Ax�AZA�mAl�AoA^5A�hAjA�FA;dAVA�A;dA=qA&�A
1'A	�^A	;dA�yA��A��A��A�-A�jAC�A�wA ��@��F@�=q@�G�@�j@��P@�o@�O�@��F@��!@��@�;d@�@�n�@�u@�1@�V@�D@�R@�$�@��@��@�7@���@� �@ߝ�@��@�7L@��@�b@�~�@�O�@�I�@���@� �@��@ӕ�@�j@�+@�J@�n�@��@љ�@���@�j@�S�@�-@��@ͩ�@�`B@̴9@���@�l�@�C�@ʇ+@���@�n�@�J@�p�@��@�?}@�@̋D@��@�5?@�ff@Η�@���@ΰ!@Η�@�@̓u@�;d@�+@�
=@���@�^5@�`B@�9X@ǅ@�C�@�M�@�X@ř�@�@ċD@î@�|�@�+@�"�@��@�+@��@�n�@�{@��h@���@���@�Ĝ@��@�Z@��;@��F@�C�@���@���@�~�@�5?@��@���@�&�@��9@�z�@�Z@�9X@��;@��@�v�@��@���@�G�@��@��@�9X@�b@��@�"�@���@��@���@�X@�?}@��@���@�b@���@�;d@��@��H@��+@�ff@�{@��@���@�V@�j@�9X@�(�@�b@��
@���@�K�@��!@�^5@��@��-@��@�X@�V@�Z@�1@��m@�ƨ@�|�@�+@���@�E�@�@��-@�?}@��@� �@��@�C�@���@���@���@�~�@�^5@�{@��h@�X@�/@��`@���@�Q�@��@�b@�1@���@��;@�ƨ@��@��@��y@���@�-@�`B@��`@�Z@�1'@�  @��;@���@�\)@�S�@�K�@�+@�ȴ@�V@�J@��T@���@�X@�V@���@�r�@�9X@��m@��@�t�@�dZ@�\)@�\)@�K�@�33@�{@��h@�p�@�hs@�O�@�/@��@���@���@���@���@��/@��9@���@�Z@�1@���@�K�@��H@���@�~�@�5?@�@��#@��^@��7@�G�@���@��@�j@�(�@���@�S�@�o@�ȴ@�V@��^@�X@��@��u@�Q�@���@��F@��F@���@�|�@�\)@��@�ȴ@���@�=q@��#@��@�X@�&�@��@���@�j@�A�@� �@��@���@�l�@�@���@�ff@�E�@��@���@���@�p�@�%@��j@��D@�I�@���@��F@��P@�K�@�"�@���@��@��!@�n�@�E�@�-@���@��-@�x�@��@��@��/@��/@��/@���@�z�@�9X@��@��w@�S�@�
=@��!@�-@�J@��@��@�7L@�%@��9@��@��u@�r�@� �@�@�w@\)@~��@~{@}�T@}�@|�@|�j@|z�@|9X@{33@z^5@y��@y�7@xĜ@x  @wK�@v�+@vE�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�$�A�oA��A��`Aڰ!Aڧ�AڶFAڡ�AړuAڕ�AڑhAڍPA�x�A�x�A�jA�+A�$�A�1A��yA��HA���A�ƨAٴ9AٶFAٲ-A٩�A٧�A٧�A٣�Aٝ�Aٝ�Aٟ�Aٛ�Aٙ�Aٝ�Aٛ�AٓuAٕ�Aٗ�AٓuAُ\AّhAّhAٍPAُ\Aُ\Aى7AًDAًDAكAمAمAفAكAمA�~�A�~�AكA�|�A�~�AفA�|�A�|�AفA�z�A�z�A�~�A�z�A�v�A�|�A�x�A�v�A�|�A�x�A�t�A�z�A�x�A�t�A�x�A�x�A�r�A�v�A�t�A�p�A�r�A�t�A�n�A�p�A�t�A�p�A�l�A�p�A�n�A�l�A�l�A�n�A�hsA�hsA�l�A�ffA�ffA�jA�jA�dZA�ffA�dZA�`BA�^5A�bNA�dZA�bNA�^5A�`BA�`BA�XA�S�A�VA�Q�A�O�A�Q�A�VA�M�A�K�A�O�A�K�A�I�A�I�A�M�A�K�A�E�A�I�A�G�A�C�A�G�A�I�A�E�A�C�A�E�A�G�A�C�A�C�A�G�A�A�A�?}A�C�A�A�A�=qA�A�A�C�A�?}A�A�A�E�A�?}A�A�A�E�A�?}A�=qA�A�A�A�A�=qA�A�A�A�A�=qA�=qA�C�A�?}A�;dA�=qA�?}A�9XA�5?A�5?A�/A�-A�1'A�1'A�+A�-A�/A�-A�(�A�-A�(�A�"�A�"�A�$�A� �A��A� �A��A��A��A��A�{A�{A��A�oA�VA�bA�{A�oA�%A�  A���A��A��yA��A��A��`A��`A��`A��HA��#A��#A��
A�ĜA���A���Aذ!Aؗ�A�p�A�\)A�G�A�=qA�7LA�1'A�-A�oA�A��A���A���A׶FA׸RA׶FAץ�Aן�Aף�Aף�AדuAׅA�|�A�l�A�bNA�bNA�XA�K�A�"�A�oA�A��A��#A�ĜA־wAֶFAֶFAֶFA֧�A֛�A֓uA։7A�n�A�dZA�G�A�oA��HAՑhA�Q�A�&�A�A��;A���AԴ9Aԙ�AԓuAԕ�AԃA�x�A�VA�O�A�C�A�1'A�"�A� �A�JA�
=A�
=A�  A��A��A��Aӧ�A�|�A�t�A�n�A�dZA�^5A�^5A�XA�VA�S�A�M�A�A�A�=qA�;dA�9XA�1'A�/A�+A�-A�&�A�$�A�$�A�$�A��A���A��HA�Aқ�A҅A�n�A�jA�dZA�S�A�?}A�=qA�?}A�=qA�7LA�9XA�;dA�;dA�5?A�9XA�9XA�-A��A��A��A�oA�%A���A��/A���Aѣ�Aї�Aѕ�Aѕ�AыDA�n�A�S�A�;dA�/A�&�A�(�A��A�JA�%A��`A�ĜAд9AУ�AУ�AН�A�v�A�E�A�33A�&�A��A�bA�A���AϑhAϋDAχ+A�z�A�l�A�`BA�XA�Q�A�C�A�;dA�5?A�(�A��A�1A���A���A��`A��#A���Aδ9AΙ�AΉ7A�x�A�K�A� �A���A���Aͧ�A�x�A�O�A�5?A��A�oA�A���A��`A��`A���A̬A̓uA�|�A�t�A�l�A�dZA�`BA�Q�A�=qA�-A��A�z�A�C�A�&�A�A���A��HA���A���A�ĜA�ƨA���AʑhA�;dA�(�A���A��mA��A���A���Aɴ9Aɛ�AɍPAɍPAɉ7A�~�A�p�A�hsA�ZA�+A��A�JA�A���A���A��yA�ƨAȑhA�C�A���A�ĜAǩ�A�x�A�G�A�5?A��A�bA�%A�A��A��A��/A�Aƥ�AƉ7A�p�A�bNA�ZA�K�A�9XA�&�A��A��A�bA�A���A���A���A���A��A��mA��HA��;A��HA��;A��/A��
A���A�AžwAź^Aź^AżjAŮA�Q�A�-A� �A�1A��;Ağ�A�-Aò-AÛ�AÉ7A�x�A�x�A�n�A�r�A�v�A�t�A�p�A�hsA�l�A�n�A�\)A�/A� �A���A���AhA�\)A�/A��A�A��A���A��9A��\A�|�A�hsA�O�A�9XA�-A�"�A��A�A��TA��RA��7A�K�A���A�bNA�33A��TA��FA��A���A���A���A���A��7A�t�A�l�A�bNA�\)A�ZA�S�A�Q�A�S�A�O�A�G�A�=qA�?}A�33A�"�A�oA��A��FA�x�A�n�A�hsA�jA�XA�;dA��A��yA�A��+A�ffA�K�A�1'A�&�A��A�bA�
=A�A���A��#A���A��!A���A���A��DA��A�r�A�O�A�-A�oA��A��yA��
A���A�/A��PA�7LA���A��uA�n�A�K�A�"�A���A��A��TA�ƨA���A�$�A��yA�dZA���A��\A�1'A��A�S�A��-A��DA�VA�
=A��#A� �A�oA��#A��9A��7A�C�A�bA��A���A��FA���A���A��A�p�A�hsA�S�A�;dA�(�A�"�A��A���A�ȴA��A�I�A���A���A�n�A�|�A�{A�r�A�A�A�"�A�A��/A���A��A�dZA�XA�G�A�E�A�A�A�A�A�?}A�/A�JA�A��yA��/A��
A��wA���A��DA�r�A�bNA�C�A��yA��A�E�A�1'A��A��A���A���A��A�`BA�?}A�JA��/A���A��A��jA���A�r�A�33A�%A��
A�A���A���A���A�~�A�?}A�{A��A���A���A�x�A�O�A��A���A�jA��A��mA��RA��uA��7A�t�A�I�A���A�&�A��9A�1'A�ȴA���A�z�A�dZA�Q�A�(�A��yA��PA�JA��HA�`BA��-A���A��uA��7A�n�A�M�A�;dA�$�A�&�A�&�A��A�JA�  A��;A�ĜA��9A���A���A���A���A���A��hA�?}A�
=A��jA�r�A�A�A�7LA�"�A�VA�A�  A��A��;A�A���A��7A�hsA�Q�A�=qA�-A��jA�+A��DA�p�A�oA��#A���A��A���A��uA���A��A�jA�\)A�VA�M�A�5?A��A�VA�%A�  A��A���A�ƨA��RA���A��hA��A�x�A�hsA�I�A��`A���A�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114                                                                                                                                                                                                                                                                        1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��Bm�BiBiDBh�Bk�BiyBh�Bh
BgBf�Bf�BgBf�Bf�Bf2Bf2Bf2Be�Be�Be�Be�Be�Be�Be�Bf�Bf�Bg8Bf�Bf�BgBf�Be�Bd�Bd�Be�Bh
BjKBiBiDBj�Bt�B�4B.B�B��B�B��B��B��B��B��B�9B��B�=B�B�VB��B�	B�B��B�[B�WB�HB�DB�B�WB�BqB+6B8BB'BYKBbNBY�BW�BU�BM�BB'B<�B6�B_B@B�B�JB�B�B�}B� B�^B�-B�nB��B��B�rBlWBL�B5�B1�B/�B%FB�B{B
�.B
��B
�|B
�vB
�B
ޞB
�'B
��B
xlB
o5B
U2B
5tB
"�B
DB	�(B	�|B	�TB	� B	�EB	�9B	�4B	��B	��B	��B	��B	��B	}"B	rGB	bB	Z�B	W�B	Q�B	B�B	B�B	5�B	3�B	4nB	(�B	�B	�B	\B	
�B	�B	�B��B��B�B�B�B�]B�9B�BԕB��B̘B�KB�EBĜB�UB�aB�B�B�B�-B�[B�!B��B��B�kB�B��B��B��B��B��B�B�xB��B�~B��B��B�B��B�XB��B��B��B��B��B�LB��B�LB��B�zB�RB��B�B�B�kB��B�qB�kB��B��B�}B��B�B��B��B��B��B��B��B��B�B��B�6B��B��B�jB��B��B��B��B�$B��B��B��B�B�nB��B��B��B�dB��B�6B�qBB�3B�B�3B�BŢBĜB�-B�3B�B��B��BȴB��B�XB��BɺB�}B��B��B��B�B��B��B	YB	!�B	9$B	<jB	>wB	@B	D�B	FB	EmB	I�B	J�B	IB	J�B	MB	OvB	OBB	PHB	OvB	OvB	Q�B	V9B	^B	d�B	j�B	j�B	h�B	jB	m�B	oiB	sMB	v�B	|�B	~(B	�4B	�B	�_B	�@B	��B	��B	��B	�	B	�!B	��B	�zB	��B	�RB	��B	�B	�wB	�[B	�nB	��B	�B	�zB	�RB	�0B	�jB	�<B	��B	��B	�OB	��B	�gB	�B	�B	ȀB	��B	˒B	�dB	͟B	�B	�<B	�BB	ԕB	��B	چB	��B	��B	�dB	ݘB	�B	�pB	�B	�TB	�,B	�2B	��B	��B	�DB	�DB	�B	�)B	�cB	�B	�B	�GB	�|B	�B	��B	�`B	��B	��B	��B	��B	�2B	�`B	�fB	��B	�DB	��B	�JB	�DB	��B	�B	�B	��B	�(B	�.B
 �B
uB
�B
uB
{B
B
GB
GB
�B
MB
�B
�B
�B
�B
B
�B
MB
�B
	�B
B
~B
JB
~B
~B
B
�B
�B
"B
�B
�B
bB
�B
oB
@B
@B
�B
FB
�B
MB
�B
SB
B
SB
SB
B
B
�B
�B
kB
�B
	B
qB
~B
�B
!B
�B
 �B
!�B
"�B
#�B
#�B
$�B
%B
&B
&B
&�B
&�B
&�B
'RB
'�B
'�B
'�B
($B
(�B
(�B
)_B
)�B
*0B
+B
+B
+B
,B
,qB
-�B
.B
.�B
/�B
0!B
0�B
1[B
1�B
1�B
1�B
2-B
33B
2�B
3�B
3�B
49B
4nB
4nB
4�B
4�B
5tB
5�B
5�B
6B
6B
6FB
6�B
7LB
7�B
7�B
7LB
7�B
7�B
8B
8�B
9�B
9XB
9�B
:�B
:�B
:�B
:�B
:�B
:�B
;dB
;0B
<B
<jB
<jB
<�B
<�B
=qB
=�B
>BB
>wB
>wB
>�B
>�B
>�B
?HB
?�B
?�B
@OB
@OB
@�B
AUB
A�B
B�B
C-B
DgB
D3B
D3B
DgB
D3B
D3B
D�B
EB
E9B
EB
E9B
F?B
F�B
F�B
GB
GzB
H�B
H�B
H�B
I�B
J�B
J�B
K)B
K�B
L�B
M6B
M�B
N<B
NG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BjBlWBg8BgmBlWBd�Bi�Bi�Bh�Bi�BiyBkBh�BiBkQBe�Bh>BqvBh�Bh>Bk�Bg�BiyBf�Be,BgmBg�BffBe�Bg�Bg8Be�Bg8Bg�Bf2Be�Bh
Bf�Be�BgBg�Be�Bf2BgmBe�Be�Bh
Be�Be`BhsBe`Be`Bf�Be�Be`BgmBf2Bd�Bg8Be�Bd�Bf�Bf�Bd�BgBgBd�Be�Bf�Bd�Bf2Bf�Bd�Be�BgmBe,Be�Bf�Be`Bd�Bf�Be`Be`BgmBe�Bd�Bf�BffBd�Be�Bf�Bd�Be,Bf�Bf2Bd�Bf�Bf�Bd�BgBf�Be`Be`Bg8Bf2BffBf�Bg�Bf�Be�Bf2BgmBf�Bf2BgmBg�Be�Be�Bg�Bg8Be�Bg�Bh
Be�Bg8Bh
Bg�Bf2BffBg�Be�BgBgmBf2Be�Bg8Bg�Be�Be`BgBg8Be�BgmBh
Be�Be�Bg�Be�Bf2Bh>BgBe�Bg�BgBe�BhsBg�Bf2BffBh
Bf2Bf�Bh
Bg�Be�Bf�Bh
BffBe`BgBh
Be,Bf2Bf2Bd�Be,BgBe�BdZBe`BffBd�Be`Bf2Bd�Bc�Be�Bf2Bc�BdZBe�Bd&Bc�Be,Bd�Bc�Bd�Be�Be`Bc�Bd�BgBe�Bd�Bf�BhsBe�Bd�Bg8Bf2Bd�BffBg�Be�Be�BiBg�Bf2BhsBjBo5Bh�Bm)Bh
Bh>BiyBi�Bj�Bi�Bl"Bk�BiyBi�Bh
Bg�BjBiDBg8Bf�Bk�BiDBhsBh�BjBg�Bg�Bg�Bp�BjBj�BhsBo�BjBiyBkQBiBiBjKBl�Bk�BkQBncBm�Bn�BpoBt�B�B{�B|�B}�B�;B}�B��B�oB.B}�B��B�iB��BcB� B�AB.B}�B��B~�B}�B.B� B�B�uB�B��B��B��B�uB�B� B�oB��B�B��B��B�B��B��B�uB�uB�uB��B��B�B��B�iB��B��B�B��B��B�B�+B�MB�%B��B�_B�YB��B��B��B��B�YB�_B�	B��B��B��B��B�DB�=B�B��B��B�B�B��B�YB�SB�B�YB�B�~B�OB�'B��B�OB��B�4B��B�$B��B�0B��B�*B��B��B�B��B�OB��B�-B��B��B��B��B�9B��B��B��B��B�hB�tB��B��B�-B�aB��B��B��B�B��B��B�B��B�nB�4B�:B��B��B��B��B�B��B��B�\B��B��B��B��B�B�\B�PB�VB�JB��B��B�	B��B�B�~B�rB��B��B��B��B��B�B��B��B��B��B�OB�B��B�qB��B�OB�0B�qB�wB��B�IB��B�'B�B�}B��B��B��B��B�qB�UB�B�zBʌB��BچB��B�HBߤB�B��B՛B�;B�B�/B�]BݘBߤB�;B�B�B�8B��B��B�KB�B�mB��B�
B��B�B�B�DB�B�B�WB�"B�KB�B�B�B��B�B�B�B��B��B� B�vB�B�AB�B�5B�B�B�	B�>B�xB
=B{B(�B&�B!-B!bB#nB'�B+�B(XB)�B)�B+�B-wB.}B-�B2�B7�B3�B8RB9$B<6B<�B?}B>wB>�B>wB>�BCaBC�BD�BG�BH�BNpBR BT,BZB]�BbNBdZBg�Bh
BtBh�Be�BgBW?BS[BU�BW�BW�BZ�BYB\]BZ�BZ�BXBX�BX�BW�BV�BV�BYBYKBV�BYBY�BXEBYB]/BOBBQBOBBM�BOBO�BN�BRTBPBK�BHBG�BE�BB'BA BB�B@�B?�B>�B@�B>wB@�B:�B:�B9�B:�B<�B=�B6zB6�B4�B0�B1�B4nBF�B:�B1[B.�B!�B�B�BSB�B�BB4B\B 'B�B'�B�BJB(B	�BkB	B iBGB�B�xB	B(B��B�B��B�B�(B��B�rB�rB�+B��B�B�B�B��B�`B�B�B�oB�GB��B��B�;B�MB��B�BB��B� B�9B�}B� B��B�BB��B��B�B��B��B��B�HB��B�[B�?B��B�BB��B�dB��B��B�B�$B��B��B�B�9B��B�B��B��B��B��B��B��B�B��B�RB�9B�zB�?B�tB�nB�'B��B��B��B��B��B��B��B�RB��B��B�	B��B��B�IB�hB��B�PB�_B��B~�BwfBv�BzxB��B��B{JBp�Bj�BZ�BR�BQNBNBI�BO�BL�BNpBU2B>�BT�B:�B4�B0�B2-B3�B0!B5�B/OB/B/B33B1'B3�B3hB0�B1�B/B-�B,B,�B+6B+kBA�B.IB8B/�B$�B \B"hB�B�B	B�B	BxB�B_BBB.B�B+�B�BBPB4BB�B�BAB
�]B
�BB
��B
�B
��B
�xB
��B
�JB
�DB
�`B
��B
��B
�ZB
�B
�%B
��B
�B
�cB
�B
�B
��B
�B
��B
�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                        4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                        4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022080601160520220806011605IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022081522331820220815223318QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022081522331820220815223318QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194620230210131946IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                